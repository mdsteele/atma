use super::config::ChecksumFormat;
use super::patch::PatchedFile;
use super::positioned::{
    PositionedBinary, PositionedChunk, PositionedRegion, PositionedSection,
};
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// Represents the final binary resulting from linking assembled object files
/// together.
pub struct LinkedBinary {
    fragments: Vec<(LinkFragment, u64)>,
    exported_symbols: HashMap<Rc<str>, u64>,
}

impl LinkedBinary {
    /// Returns the total size of the binary, in bytes.
    pub fn size(&self) -> u64 {
        match self.fragments.last() {
            None => 0,
            Some(&(_, frag_end)) => frag_end,
        }
    }

    /// Returns the byte offset into the binary for the given fully-qualified
    /// symbol name, or `None` if that symbol doesn't have a meaningful
    /// location in the binary.
    pub fn get_symbol_offset(&self, name: &str) -> Option<u64> {
        self.exported_symbols.get(name).copied()
    }

    pub(super) fn from_patched_files(
        patched_files: Vec<PatchedFile>,
        positioned_binary: &PositionedBinary,
    ) -> LinkedBinary {
        let mut builder = FragmentBuilder {
            total_size: 0,
            patched_files,
            fragments: Vec::new(),
            exported_symbols: HashMap::new(),
        };
        for region in &positioned_binary.regions {
            builder.add_region(region);
        }
        LinkedBinary::new(builder.fragments, builder.exported_symbols)
    }

    fn new(
        fragments: Vec<LinkFragment>,
        exported_symbols: HashMap<Rc<str>, u64>,
    ) -> LinkedBinary {
        let mut frag_end: u64 = 0;
        let fragments = fragments
            .into_iter()
            .map(|fragment| {
                frag_end += fragment.size();
                (fragment, frag_end)
            })
            .collect::<Vec<(LinkFragment, u64)>>();
        LinkedBinary { fragments, exported_symbols }
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
}

//===========================================================================//

/// Represents one piece of a final binary file.
#[derive(Debug, Eq, PartialEq)]
pub(super) enum LinkFragment {
    /// A slice of data that should be written into the binary.
    Data {
        /// The data to write.
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

struct FragmentBuilder {
    total_size: u64,
    patched_files: Vec<PatchedFile>,
    fragments: Vec<LinkFragment>,
    exported_symbols: HashMap<Rc<str>, u64>,
}

impl FragmentBuilder {
    fn add_region(&mut self, region: &PositionedRegion) {
        let region_end = self.total_size + region.binary_size;
        for section in &region.sections {
            self.fill_to(section.binary_offset, region.fill);
            self.add_section(section);
        }
        self.fill_to(region_end, region.fill);
    }

    fn add_section(&mut self, section: &PositionedSection) {
        let section_end = self.total_size + section.binary_size;
        for chunk in &section.chunks {
            self.fill_to(chunk.binary_offset, section.fill);
            self.add_chunk(chunk);
        }
        self.fill_to(section_end, section.fill);
    }

    fn add_chunk(&mut self, chunk: &PositionedChunk) {
        let chunk_end = self.total_size + chunk.binary_size;
        let patched = &mut self.patched_files[chunk.id.object_index].chunks
            [chunk.id.chunk_index];
        for (name, symbol_offset) in &patched.exported_symbols {
            self.exported_symbols.insert(
                name.clone(),
                chunk.binary_offset + u64::from(*symbol_offset),
            );
        }
        let data = Vec::from(patched.take_data());
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
    use std::collections::HashMap;
    use std::io::Cursor;

    fn binary_from_fragments(fragments: Vec<LinkFragment>) -> LinkedBinary {
        LinkedBinary::new(fragments, HashMap::new())
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
