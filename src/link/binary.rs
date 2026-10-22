use super::config::ChecksumFormat;
use super::patch::PatchedFile;
use super::positioned::{
    PositionedBinary, PositionedChunk, PositionedRegion, PositionedSection,
    PositionedSymbol,
};
use crate::addr::{Addr, BitWidth};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// Represents a symbol in a linked binary.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct BinarySymbol {
    /// The fully qualified name of the symbol.
    ///
    /// Note that a linked binary can contain multiple symbols with the same
    /// name, but `is_external` will be true for at most one of those symbols.
    pub name: Rc<str>,
    /// The name of the address space that the symbol exists in.
    pub space: Rc<str>,
    /// The bit width of the symbol address.
    pub bit_width: BitWidth,
    /// The address of the symbol within its address space.
    pub address: Addr,
    /// The byte offset into the binary for the symbol, or `None` if the symbol
    /// doesn't have a meaningful location in the binary.
    pub binary_offset: Option<u64>,
    /// True if this symbol is external.  A linked binary will not contain two
    /// external symbols with the same `name`.
    pub is_external: bool,
}

//===========================================================================//

/// Represents the final binary resulting from linking assembled object files
/// together.
pub struct LinkedBinary {
    fragments: Vec<(LinkFragment, u64)>,
    symbols: Vec<BinarySymbol>,
    external_symbol_offsets: HashMap<Rc<str>, u64>,
}

impl LinkedBinary {
    /// Returns the total size of the binary, in bytes.
    pub fn size(&self) -> u64 {
        match self.fragments.last() {
            None => 0,
            Some(&(_, frag_end)) => frag_end,
        }
    }

    /// Returns a list of all symbols in the binary.
    pub fn symbols(&self) -> &[BinarySymbol] {
        &self.symbols
    }

    /// Returns the byte offset into the binary for the given fully-qualified
    /// symbol name, or `None` if that symbol doesn't have a meaningful
    /// location in the binary.
    pub fn get_symbol_offset(&self, name: &str) -> Option<u64> {
        self.external_symbol_offsets.get(name).copied()
    }

    pub(super) fn from_patched_files(
        patched_files: Vec<PatchedFile>,
        positioned_binary: &PositionedBinary,
    ) -> LinkedBinary {
        let mut builder = BinaryBuilder {
            total_size: 0,
            patched_files,
            fragments: Vec::new(),
            symbols: Vec::new(),
        };
        for region in &positioned_binary.regions {
            builder.add_region(region);
        }
        builder.collect_symbols(positioned_binary);
        LinkedBinary::new(builder.fragments, builder.symbols)
    }

    fn new(
        fragments: Vec<LinkFragment>,
        symbols: Vec<BinarySymbol>,
    ) -> LinkedBinary {
        let mut frag_end: u64 = 0;
        let fragments = fragments
            .into_iter()
            .map(|fragment| {
                frag_end += fragment.size();
                (fragment, frag_end)
            })
            .collect::<Vec<(LinkFragment, u64)>>();
        let external_symbol_offsets = symbols
            .iter()
            .filter(|symbol| symbol.is_external)
            .filter_map(|symbol| {
                symbol
                    .binary_offset
                    .map(|offset| (symbol.name.clone(), offset))
            })
            .collect::<HashMap<Rc<str>, u64>>();
        LinkedBinary { fragments, symbols, external_symbol_offsets }
    }

    pub(super) fn calculate_checksum(
        &self,
        start: u64,
        end: u64,
        unit_format: ChecksumFormat,
    ) -> u64 {
        debug_assert!(start <= end);
        debug_assert!(end <= self.size());
        let mut checksum: u64 = 0;
        let mut consumed: u64 = 0;
        let mut fragment_index: usize =
            self.fragments.partition_point(|&(_, frag_end)| frag_end <= start);
        while start + consumed < end {
            debug_assert!(fragment_index < self.fragments.len());
            let (ref fragment, frag_end) = self.fragments[fragment_index];
            let frag_size = fragment.size();
            debug_assert!(frag_end >= frag_size);
            let frag_start = frag_end - frag_size;
            debug_assert!(start + consumed >= frag_start);
            let sub_start = start + consumed - frag_start;
            let sub_end = end.min(frag_end) - frag_start;
            checksum = checksum.wrapping_add(fragment.calculate_checksum(
                sub_start,
                sub_end,
                unit_format,
                consumed,
            ));
            consumed += sub_end - sub_start;
            fragment_index += 1;
        }
        checksum
    }

    pub(super) fn store_checksum(
        &mut self,
        checksum: u64,
        format: ChecksumFormat,
        dest: u64,
    ) {
        debug_assert!(dest + u64::from(format.size()) <= self.size());
        match format {
            ChecksumFormat::U8 => self.set_byte(dest, checksum as u8),
            ChecksumFormat::U8Cpl => self.set_byte(dest, !checksum as u8),
            ChecksumFormat::U16be => {
                self.set_byte(dest, (checksum >> 8) as u8);
                self.set_byte(dest + 1, checksum as u8);
            }
            ChecksumFormat::U16beCpl => {
                self.set_byte(dest, (!checksum >> 8) as u8);
                self.set_byte(dest + 1, !checksum as u8);
            }
            ChecksumFormat::U16le => {
                self.set_byte(dest, checksum as u8);
                self.set_byte(dest + 1, (checksum >> 8) as u8);
            }
            ChecksumFormat::U16leCpl => {
                self.set_byte(dest, !checksum as u8);
                self.set_byte(dest + 1, (!checksum >> 8) as u8);
            }
        }
    }

    fn set_byte(&mut self, dest: u64, new_byte: u8) {
        debug_assert!(dest < self.size());
        let fragment_index: usize =
            self.fragments.partition_point(|&(_, frag_end)| frag_end <= dest);
        debug_assert!(fragment_index < self.fragments.len());
        match self.fragments[fragment_index] {
            (LinkFragment::Data { ref mut data }, frag_end) => {
                debug_assert!(dest < frag_end);
                debug_assert!(data.len() as u64 <= frag_end);
                let frag_start = frag_end - data.len() as u64;
                debug_assert!(dest >= frag_start);
                data[(dest - frag_start) as usize] = new_byte;
            }
            (LinkFragment::Fill { size, byte: fill_byte }, frag_end) => {
                if new_byte == fill_byte {
                    return;
                }
                debug_assert!(dest < frag_end);
                debug_assert!(size <= frag_end);
                let frag_start = frag_end - size;
                debug_assert!(dest >= frag_start);
                if size == 1 {
                    self.fragments[fragment_index].0 =
                        LinkFragment::Data { data: vec![new_byte] };
                } else if dest == frag_start {
                    if fragment_index > 0
                        && let (
                            LinkFragment::Data { ref mut data },
                            ref mut data_end,
                        ) = self.fragments[fragment_index - 1]
                    {
                        data.push(new_byte);
                        *data_end += 1;
                        self.fragments[fragment_index].0 =
                            LinkFragment::Fill {
                                size: size - 1,
                                byte: fill_byte,
                            };
                    } else {
                        self.fragments[fragment_index].0 =
                            LinkFragment::Fill {
                                size: size - 1,
                                byte: fill_byte,
                            };
                        self.fragments.insert(
                            fragment_index,
                            (
                                LinkFragment::Data { data: vec![new_byte] },
                                frag_start + 1,
                            ),
                        );
                    }
                } else {
                    self.fragments[fragment_index] = (
                        LinkFragment::Fill {
                            size: dest - frag_start,
                            byte: fill_byte,
                        },
                        dest,
                    );
                    self.fragments.insert(
                        fragment_index + 1,
                        (
                            LinkFragment::Data { data: vec![new_byte] },
                            dest + 1,
                        ),
                    );
                    let remaining = frag_end - (dest + 1);
                    if remaining > 0 {
                        self.fragments.insert(
                            fragment_index + 2,
                            (
                                LinkFragment::Fill {
                                    size: remaining,
                                    byte: fill_byte,
                                },
                                frag_end,
                            ),
                        );
                    }
                }
            }
        }
    }

    /// Writes the binary to an output writer.
    pub fn write_to(&self, writer: &mut impl io::Write) -> io::Result<()> {
        let mut fill_buffer = [0u8; 1024];
        for (fragment, _) in &self.fragments {
            match fragment {
                LinkFragment::Data { data } => {
                    writer.write_all(data)?;
                }
                &LinkFragment::Fill { size, byte } => {
                    if fill_buffer[0] != byte {
                        let fill_len = size.min(fill_buffer.len() as u64);
                        fill_buffer[..fill_len as usize].fill(byte);
                    }
                    let mut remaining = size;
                    while remaining > 0 {
                        let write_len =
                            remaining.min(fill_buffer.len() as u64);
                        writer
                            .write_all(&fill_buffer[..write_len as usize])?;
                        remaining -= write_len;
                    }
                }
            }
        }
        Ok(())
    }

    /// Writes the binary's symbol table to an output writer in a plain-text
    /// format.
    pub fn write_symbols_to(
        &self,
        writer: &mut impl io::Write,
    ) -> io::Result<()> {
        let offset_digits: usize =
            self.size().bit_width().div_ceil(4).max(1) as usize;
        let mut max_space_chars: usize = 1;
        let mut max_addr_digits: usize = 1;
        for symbol in &self.symbols {
            max_space_chars =
                max_space_chars.max(symbol.space.chars().count());
            max_addr_digits =
                max_addr_digits.max(symbol.bit_width.hex_digits());
        }
        for symbol in &self.symbols {
            let offset_string = if let Some(offset) = symbol.binary_offset {
                format!("${0:01$x}", offset, offset_digits)
            } else {
                format!(" {0:1$}", "", offset_digits)
            };
            let addr_string = format!(
                "${0:01$x}",
                symbol.address,
                symbol.bit_width.hex_digits()
            );
            writeln!(
                writer,
                "{offset_string} | {0:max_space_chars$} | \
                 {addr_string:>01$} | {2} {3}",
                symbol.space,
                1 + max_addr_digits,
                if symbol.is_external { '+' } else { '-' },
                symbol.name
            )?;
        }
        Ok(())
    }
}

//===========================================================================//

/// Represents one piece of a final binary file.
#[derive(Debug, Eq, PartialEq)]
enum LinkFragment {
    /// A slice of data that should be written into the binary.
    Data {
        /// The data to write.
        ///
        /// This is a `Vec<u8>` rather than `Box<[u8]>` because it sometimes
        /// needs to be resized when writing checksums into the linked binary.
        data: Vec<u8>,
    },
    /// A span of the given size that should be filled with the given byte.
    Fill {
        /// The length of the fill span, in bytes.
        size: u64,
        /// The byte value to fill this span with.
        byte: u8,
    },
}

impl LinkFragment {
    /// Returns the size of this fragment, in bytes.
    fn size(&self) -> u64 {
        match self {
            Self::Data { data } => data.len() as u64,
            &Self::Fill { size, .. } => size,
        }
    }

    /// Calculates a checksum over a portion of this fragment.  The `start` and
    /// `end` offsets are relative to the start of the fragment.  `after`
    /// indicates the number of bytes between the start of the entire checksum
    /// range and the beginning of the `start` to `end` range in this fragment.
    fn calculate_checksum(
        &self,
        start: u64,
        end: u64,
        unit_format: ChecksumFormat,
        after: u64,
    ) -> u64 {
        debug_assert!(start <= end);
        match self {
            LinkFragment::Data { data } => {
                debug_assert!(end <= (data.len() as u64));
                let mut checksum: u64 = 0;
                match unit_format {
                    ChecksumFormat::U8 => {
                        for &byte in &data[(start as usize)..(end as usize)] {
                            checksum = checksum.wrapping_add(u64::from(byte));
                        }
                    }
                    ChecksumFormat::U8Cpl => {
                        for &byte in &data[(start as usize)..(end as usize)] {
                            checksum = checksum.wrapping_add(u64::from(!byte));
                        }
                    }
                    ChecksumFormat::U16be => {
                        let mut hi: bool = after.is_multiple_of(2);
                        for &byte in &data[(start as usize)..(end as usize)] {
                            let value = if hi {
                                u64::from(byte) << 8
                            } else {
                                u64::from(byte)
                            };
                            checksum = checksum.wrapping_add(value);
                            hi = !hi;
                        }
                    }
                    other => todo!("{other:?}"),
                }
                checksum
            }
            &LinkFragment::Fill { size, byte } => {
                debug_assert!(end <= size);
                let count = end - start;
                match unit_format {
                    ChecksumFormat::U8 => count.wrapping_mul(u64::from(byte)),
                    ChecksumFormat::U8Cpl => {
                        count.wrapping_mul(u64::from(!byte))
                    }
                    ChecksumFormat::U16be => {
                        let num_hi = count / 2 + ((count % 2) & !(after % 2));
                        let num_lo = count / 2 + ((count % 2) & (after % 2));
                        let hi_sum = num_hi.wrapping_mul(u64::from(byte) << 8);
                        let lo_sum = num_lo.wrapping_mul(u64::from(byte));
                        hi_sum.wrapping_add(lo_sum)
                    }
                    other => todo!("{other:?}"),
                }
            }
        }
    }
}

//===========================================================================//

struct BinaryBuilder {
    total_size: u64,
    patched_files: Vec<PatchedFile>,
    fragments: Vec<LinkFragment>,
    symbols: Vec<BinarySymbol>,
}

impl BinaryBuilder {
    fn collect_symbols(&mut self, positioned_binary: &PositionedBinary) {
        self.add_symbols(&positioned_binary.external_symbols, true);
        for internal_symbols in &positioned_binary.internal_symbols {
            self.add_symbols(internal_symbols, false);
        }
        self.symbols.sort_by(Self::symbol_ordering);
    }

    fn add_symbols(
        &mut self,
        symbol_map: &HashMap<Rc<str>, PositionedSymbol>,
        is_external: bool,
    ) {
        self.symbols.extend(symbol_map.iter().map(|(name, symbol)| {
            BinarySymbol {
                name: name.clone(),
                space: symbol.absolute.space.name.clone(),
                bit_width: symbol.absolute.space.bit_width,
                address: symbol.absolute.address,
                binary_offset: symbol.binary_offset,
                is_external,
            }
        }));
    }

    fn symbol_ordering(lhs: &BinarySymbol, rhs: &BinarySymbol) -> Ordering {
        lhs.binary_offset
            .cmp(&rhs.binary_offset)
            .then_with(|| lhs.space.cmp(&rhs.space))
            .then_with(|| lhs.address.cmp(&rhs.address))
            .then_with(|| rhs.is_external.cmp(&lhs.is_external))
            .then_with(|| lhs.name.cmp(&rhs.name))
    }

    fn add_region(&mut self, region: &PositionedRegion) {
        let region_end = self.total_size + region.binary_size;
        for section in &region.sections {
            if let Some(binary_offset) = section.binary_offset {
                self.fill_to(binary_offset, region.fill);
                self.add_section(section);
            }
        }
        self.fill_to(region_end, region.fill);
    }

    fn add_section(&mut self, section: &PositionedSection) {
        let section_end = self.total_size + section.binary_size;
        for chunk in &section.chunks {
            if let Some(binary_offset) = chunk.binary_offset {
                self.fill_to(binary_offset, section.fill);
                self.add_chunk(chunk);
            }
        }
        self.fill_to(section_end, section.fill);
    }

    fn add_chunk(&mut self, chunk: &PositionedChunk) {
        let chunk_end = self.total_size + chunk.binary_size;
        let patched = &mut self.patched_files[chunk.id.object_index].chunks
            [chunk.id.chunk_index];
        let data = Vec::from(std::mem::take(&mut patched.data));
        self.total_size += data.len() as u64;
        self.fragments.push(LinkFragment::Data { data });
        self.fill_to(chunk_end, chunk.fill);
    }

    fn fill_to(&mut self, offset: u64, byte: u8) {
        let size = offset.strict_sub(self.total_size);
        if size > 0 {
            self.fragments.push(LinkFragment::Fill { size, byte });
        }
        self.total_size = offset;
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{LinkFragment, LinkedBinary};
    use crate::link::ChecksumFormat;
    use std::io::Cursor;

    fn binary_from_fragments(fragments: Vec<LinkFragment>) -> LinkedBinary {
        LinkedBinary::new(fragments, Vec::new())
    }

    fn binary_to_vec(binary: &LinkedBinary) -> Vec<u8> {
        let mut cursor = Cursor::new(Vec::<u8>::new());
        binary.write_to(&mut cursor).unwrap();
        cursor.into_inner()
    }

    #[test]
    fn calculate_data_fragment_checksum() {
        let data = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let fragment = LinkFragment::Data { data };
        assert_eq!(
            fragment.calculate_checksum(0, 8, ChecksumFormat::U8, 0),
            0x1c
        );
        assert_eq!(
            fragment.calculate_checksum(1, 3, ChecksumFormat::U8Cpl, 0),
            0x1fb
        );
        assert_eq!(
            fragment.calculate_checksum(0, 8, ChecksumFormat::U16be, 1),
            0x100c
        );
    }

    #[test]
    fn calculate_fill_fragment_checksum() {
        let fragment = LinkFragment::Fill { size: 8, byte: 3 };
        assert_eq!(
            fragment.calculate_checksum(0, 8, ChecksumFormat::U8, 0),
            0x18
        );
        assert_eq!(
            fragment.calculate_checksum(1, 3, ChecksumFormat::U8Cpl, 0),
            0x1f8
        );
        assert_eq!(
            fragment.calculate_checksum(0, 8, ChecksumFormat::U16be, 0),
            0xc0c
        );
        assert_eq!(
            fragment.calculate_checksum(0, 8, ChecksumFormat::U16be, 1),
            0xc0c
        );
        assert_eq!(
            fragment.calculate_checksum(0, 7, ChecksumFormat::U16be, 0),
            0xc09
        );
        assert_eq!(
            fragment.calculate_checksum(0, 7, ChecksumFormat::U16be, 1),
            0x90c
        );
    }

    #[test]
    fn binary_size() {
        assert_eq!(binary_from_fragments(vec![]).size(), 0);
        let binary = binary_from_fragments(vec![
            LinkFragment::Fill { size: 3000, byte: 10 },
            LinkFragment::Data { data: vec![1, 2, 3, 4] },
        ]);
        assert_eq!(binary.size(), 3004);
    }

    #[test]
    fn binary_set_byte() {
        for index in 0..16 {
            let mut binary = binary_from_fragments(vec![
                LinkFragment::Fill { size: 3, byte: 10 },
                LinkFragment::Data { data: vec![1, 2, 3] },
                LinkFragment::Fill { size: 2, byte: 20 },
                LinkFragment::Data { data: vec![4, 5, 6] },
                LinkFragment::Fill { size: 1, byte: 30 },
                LinkFragment::Data { data: vec![7] },
                LinkFragment::Fill { size: 3, byte: 40 },
            ]);
            let mut expected =
                vec![10, 10, 10, 1, 2, 3, 20, 20, 4, 5, 6, 30, 7, 40, 40, 40];
            assert_eq!(binary_to_vec(&binary), expected);
            expected[index] = 99;
            binary.set_byte(index as u64, 99);
            assert_eq!(binary_to_vec(&binary), expected);
        }
    }
}

//===========================================================================//
