use super::config::{MemoryConfig, SectionConfig};
use super::error::LinkError;
use crate::bus::{Addr, Align, Offset, Range, Size};
use crate::obj::{ObjChunk, ObjPatch, ObjSymbol};
use rangemap::RangeInclusiveSet;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// Represents a data chunk that has been arranged within its section.
pub struct ArrangedChunk {
    /// The byte offset of this chunk, relative to the starting address of its
    /// section.
    pub offset: Offset,
    /// Static data (before patches are applied) at the start of this chunk.
    pub data: Rc<[u8]>,
    /// Relative symbols defined in this chunk.
    pub symbols: Rc<[ObjSymbol]>,
    /// Patches to apply to this chunk's data when linking.
    pub patches: Rc<[ObjPatch]>,
}

impl ArrangedChunk {
    pub(crate) fn with_offset(
        chunk: &ObjChunk,
        offset: Offset,
    ) -> ArrangedChunk {
        debug_assert!(offset >= Offset::ZERO);
        ArrangedChunk {
            offset,
            data: chunk.data.clone(),
            symbols: chunk.symbols.clone(),
            patches: chunk.patches.clone(),
        }
    }
}

//===========================================================================//

/// Represents a data section after all chunks have been arranged within the
/// section.
pub struct ArrangedSection {
    /// The required alignment for this section, within its address space,
    /// after taking chunk placement restrictions into account.
    pub align: Align,
    /// The total size of this section's data, in bytes.
    pub size: Size,
    /// The chunks in this section.
    pub chunks: Rc<[ArrangedChunk]>,
}

impl ArrangedSection {
    /// Given the list of chunks that belong to this section, arranges the
    /// chunks relative to the eventual starting address of the section.
    pub fn arrange_chunks(
        section: &SectionConfig,
        chunks: &[&ObjChunk],
    ) -> Result<ArrangedSection, LinkError> {
        let mut sorted = Vec::from(chunks);
        sorted.sort_by(|a, b| b.align.cmp(&a.align));
        let mut range_set = RangeInclusiveSet::<Addr>::new();
        let mut arranged = Vec::<ArrangedChunk>::with_capacity(chunks.len());
        let mut section_align = section.align;
        for chunk in sorted {
            section_align = section_align.max(chunk.align);
            if let Some(within) = chunk.within {
                section_align = section_align.max(within);
            }
            if u64::from(chunk.size) < chunk.data.len() as u64 {
                return Err(LinkError::Misc); // TODO: add error details
            }
            if let Some(within) = chunk.within
                && chunk.size > Size::from(within)
            {
                return Err(LinkError::Misc); // TODO: add error details
            }
            if chunk.size == Size::ZERO {
                arranged.push(ArrangedChunk::with_offset(chunk, Offset::ZERO));
                continue;
            }
            let range = try_place(
                &range_set,
                Range::FULL,
                chunk.size,
                chunk.align,
                chunk.within,
            )
            .ok_or(LinkError::Misc)?; // TODO: add error details
            arranged.push(ArrangedChunk::with_offset(
                chunk,
                range.start() - Addr::MIN,
            ));
            range_set.insert(range.into());
        }
        debug_assert_eq!(arranged.len(), chunks.len());
        let size = range_set
            .last()
            .map(|r| Size::of_range(&(Addr::MIN..=*r.end())))
            .unwrap_or(Size::ZERO);
        Ok(ArrangedSection {
            align: section_align,
            size,
            chunks: Rc::from(arranged),
        })
    }
}

//===========================================================================//

/// Represents a data chunk that has been arranged within its positioned within
/// its memory region.
pub struct PositionedChunk {
    /// The absolute starting address of this chunk within its address space.
    pub start: Addr,
    /// Static data (before patches are applied) at the start of this chunk.
    pub data: Rc<[u8]>,
    /// Symbols defined in this chunk, mapped to their absolute addresses.
    pub symbols: HashMap<Rc<str>, Addr>,
    /// Patches to apply to this chunk's data when linking.
    pub patches: Rc<[ObjPatch]>,
}

impl PositionedChunk {
    pub(crate) fn with_section_start(
        chunk: &ArrangedChunk,
        section_start: Addr,
    ) -> PositionedChunk {
        let chunk_start = section_start + chunk.offset;
        PositionedChunk {
            start: chunk_start,
            data: chunk.data.clone(),
            symbols: chunk
                .symbols
                .iter()
                .map(|symbol| {
                    (symbol.name.clone(), chunk_start + symbol.offset)
                })
                .collect(),
            patches: chunk.patches.clone(),
        }
    }
}

//===========================================================================//

/// Represents a data section that has been positioned within its memory
/// region.
pub struct PositionedSection {
    /// The absolute starting address of this section within its address space.
    pub start: Addr,
    /// The total size of this section's data, in bytes.
    pub size: Size,
    /// The chunks in this section.
    pub chunks: Rc<[PositionedChunk]>,
}

impl PositionedSection {
    pub(crate) fn with_start(
        section: &ArrangedSection,
        start: Addr,
    ) -> PositionedSection {
        PositionedSection {
            start,
            size: section.size,
            chunks: section
                .chunks
                .iter()
                .map(|chunk| PositionedChunk::with_section_start(chunk, start))
                .collect(),
        }
    }
}

//===========================================================================//

/// Represents a memory region after its sections have been positioned within
/// its address space.
pub struct PositionedMemory {
    /// The address of the start of this memory region.
    pub start: Addr,
    /// The size of this memory region, in bytes.
    pub size: Size,
    /// The sections in this memory region.
    pub sections: Rc<[PositionedSection]>,
}

impl PositionedMemory {
    /// Given the list of sections that belong to this memory region, positions
    /// those sections within the memory region.
    pub fn position_sections(
        memory: &MemoryConfig,
        sections: &[&ArrangedSection],
    ) -> Result<PositionedMemory, LinkError> {
        let memory_range: Option<Range> =
            memory.start.range_with_size(memory.size);
        let mut range_set = RangeInclusiveSet::<Addr>::new();
        let mut positioned =
            Vec::<PositionedSection>::with_capacity(sections.len());
        for section in sections {
            if section.size == Size::ZERO {
                positioned.push(PositionedSection::with_start(
                    section,
                    memory.start,
                ));
            } else if let Some(outer_range) = memory_range {
                let range = try_place(
                    &range_set,
                    outer_range,
                    section.size,
                    section.align,
                    None,
                )
                .ok_or(LinkError::Misc)?; // TODO: add error details
                positioned.push(PositionedSection::with_start(
                    section,
                    range.start(),
                ));
                range_set.insert(range.into());
            } else {
                // TODO: Error: Cannot place a non-zero-size section in
                // zero-size memory region.
                return Err(LinkError::Misc);
            }
        }
        debug_assert_eq!(positioned.len(), sections.len());
        Ok(PositionedMemory {
            start: memory.start,
            size: memory.size,
            sections: Rc::from(positioned),
        })
    }
}

//===========================================================================//

fn try_place(
    range_set: &RangeInclusiveSet<Addr>,
    outer_range: Range,
    size: Size,
    align: Align,
    opt_within: Option<Align>,
) -> Option<Range> {
    debug_assert_ne!(size, Size::ZERO);
    for gap in range_set.gaps(&outer_range.into()) {
        let gap = Range::try_from(&gap).unwrap();
        if let Some(within) = opt_within
            && within > align
        {
            for subgap in gap.split_at(within) {
                if let Some(start) = subgap.first_aligned_to(align)
                    && let Some(range) = start.range_with_size(size)
                    && subgap.contains(range.end())
                {
                    return Some(range);
                }
            }
        } else if let Some(start) = gap.first_aligned_to(align)
            && let Some(range) = start.range_with_size(size)
            && gap.contains(range.end())
        {
            return Some(range);
        }
    }
    None
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::try_place;
    use crate::bus::{Addr, Align, Range, Size};
    use rangemap::RangeInclusiveSet;

    #[test]
    fn place_in_empty_range_set() {
        let range_set = RangeInclusiveSet::new();
        let size = Size::from(0x100u32);
        let align = Align::default();
        let range = try_place(&range_set, Range::FULL, size, align, None);
        let expected = Addr::MIN.range_with_size(size).unwrap();
        assert_eq!(range, Some(expected));
    }

    #[test]
    fn place_full_addr_range_in_empty_range_set() {
        let range_set = RangeInclusiveSet::new();
        let size = Size::MAX;
        let align = Align::default();
        let range = try_place(&range_set, Range::FULL, size, align, None);
        let expected = Addr::MIN.range_with_size(size).unwrap();
        assert_eq!(range, Some(expected));
    }

    #[test]
    fn try_place_full_addr_range_in_nonempty_range_set() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(Addr::MIN..=Addr::MIN);
        let size = Size::MAX;
        let align = Align::default();
        let range = try_place(&range_set, Range::FULL, size, align, None);
        assert_eq!(range, None);
    }

    #[test]
    fn place_aligned() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(Addr::from(0x00u16)..=Addr::from(0x03u16));
        range_set.insert(Addr::from(0x16u16)..=Addr::from(0x18u16));
        range_set.insert(Addr::from(0x28u16)..=Addr::from(0x2fu16));
        let size = Size::from(0x7u32);
        let align = Align::try_from(0x10).unwrap();
        let range = try_place(&range_set, Range::FULL, size, align, None);
        let expected = Addr::from(0x20u16).range_with_size(size).unwrap();
        assert_eq!(range, Some(expected));
    }

    #[test]
    fn place_within() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(Addr::from(0x070u16)..=Addr::from(0x18fu16));
        range_set.insert(Addr::from(0x270u16)..=Addr::from(0x274u16));
        range_set.insert(Addr::from(0x4a0u16)..=Addr::from(0x4ffu16));
        let size = Size::from(0x7fu32);
        let align = Align::default();
        let within = Some(Align::try_from(0x100).unwrap());
        let start = try_place(&range_set, Range::FULL, size, align, within);
        let expected = Addr::from(0x275u16).range_with_size(size).unwrap();
        assert_eq!(start, Some(expected));
    }
}

//===========================================================================//
