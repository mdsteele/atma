use super::config::LinkConfig;
use super::error::LinkError;
use super::loose::{ChunkId, LooseChunk, LooseSection};
use super::place::try_place;
use crate::addr::{Addr, Align, Offset, Range, Size};
use rangemap::RangeInclusiveSet;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// Represents a data chunk that has been arranged within its section.
pub struct ArrangedChunk {
    /// The ID for this chunk.
    pub id: ChunkId,
    /// The byte offset of this chunk, relative to the starting address of its
    /// section.
    pub offset: Offset,
}

impl ArrangedChunk {
    pub fn with_offset(chunk: LooseChunk, offset: Offset) -> ArrangedChunk {
        debug_assert!(offset >= Offset::ZERO);
        ArrangedChunk { id: chunk.id, offset }
    }
}

//===========================================================================//

/// Represents a data section after all chunks have been arranged within the
/// section.
pub struct ArrangedSection {
    /// The name of this section.
    pub name: Rc<str>,
    /// The name of the memory region that this section should be loaded into.
    pub load: Rc<str>,
    /// If set, then the section must start at exactly this address.
    pub start: Option<Addr>,
    /// The required alignment for this section, within its address space,
    /// after taking chunk placement restrictions into account.
    pub align: Align,
    /// If set, then this entire section must not cross any alignment boundary
    /// of this size within its address space.
    pub within: Option<Align>,
    /// The total size of this section's data, in bytes.
    pub size: Size,
    /// The chunks in this section.
    pub chunks: Vec<ArrangedChunk>,
}

impl ArrangedSection {
    /// Arranges the chunks in each section relative to the eventual starting
    /// address of that section.
    pub fn arrange(
        sections: Vec<LooseSection>,
        errors: &mut Vec<LinkError>,
    ) -> Vec<ArrangedSection> {
        sections
            .into_iter()
            .map(|section| ArrangedSection::arrange_one(section, errors))
            .collect::<Vec<ArrangedSection>>()
    }

    /// Arranges the chunks relative to the eventual starting address of the
    /// section.
    fn arrange_one(
        mut section: LooseSection,
        errors: &mut Vec<LinkError>,
    ) -> ArrangedSection {
        section.chunks.sort_by(|a, b| b.align.cmp(&a.align));
        let mut range_set = RangeInclusiveSet::<Addr>::new();
        let mut arranged =
            Vec::<ArrangedChunk>::with_capacity(section.chunks.len());
        for chunk in section.chunks {
            if let Some(within) = chunk.within {
                // If the chunk is larger than its `within` constraint, then we
                // cannot possibly place it without violating that constraint.
                if chunk.size > Size::from(within) {
                    errors.push(LinkError::ChunkCannotBePlaced);
                    continue;
                }
                // Specifhing a `within` constraint for a chunk forces the
                // whole section to at least that alignment, so that we can
                // place the chunk relative to the section's start address
                // without yet knowing that address.
                section.align = section.align.max(within);
            }
            // Specifhing an alignment for a chunk forces the whole section to
            // at least that alignment, so that we can place the chunk relative
            // to the section's start address without yet knowing that address.
            section.align = section.align.max(chunk.align);
            // If the chunk has a size of zero, then it can always be placed at
            // the start of the section (since the section's alignment has been
            // updated to at least that of the chunk's).
            if chunk.size == Size::ZERO {
                arranged.push(ArrangedChunk::with_offset(chunk, Offset::ZERO));
                continue;
            }
            if let Some(range) = try_place(
                &range_set,
                Range::FULL,
                chunk.size,
                None,
                chunk.align,
                chunk.within,
            ) {
                arranged.push(ArrangedChunk::with_offset(
                    chunk,
                    range.start() - Addr::MIN,
                ));
                range_set.insert(range.into());
            } else {
                errors.push(LinkError::ChunkCannotBePlaced);
            }
        }
        let section_size = range_set
            .last()
            .map(|r| Range::with_bounds(Addr::MIN, *r.end()).size())
            .unwrap_or(Size::ZERO);
        ArrangedSection {
            name: section.name,
            load: section.load,
            start: section.start,
            align: section.align,
            within: section.within,
            size: section_size,
            chunks: arranged,
        }
    }
}

//===========================================================================//

pub struct ArrangedRegion {
    /// The range of addresses covered by this memory region.
    pub range: Range,
    /// The sections in this memory region.
    pub sections: Vec<ArrangedSection>,
}

impl ArrangedRegion {
    pub fn collect(
        config: &LinkConfig,
        all_sections: Vec<ArrangedSection>,
        _errors: &mut Vec<LinkError>,
    ) -> Vec<ArrangedRegion> {
        let mut arranged_regions =
            Vec::<ArrangedRegion>::with_capacity(config.memory.len());
        let mut region_name_to_index = HashMap::<Rc<str>, usize>::new();
        for memory_region in &config.memory {
            region_name_to_index
                .insert(memory_region.name.clone(), arranged_regions.len());
            arranged_regions.push(ArrangedRegion {
                range: memory_region.range,
                sections: Vec::new(),
            });
        }
        for section in all_sections {
            let region_index = region_name_to_index[&section.load];
            arranged_regions[region_index].sections.push(section);
        }
        arranged_regions
    }
}

//===========================================================================//
