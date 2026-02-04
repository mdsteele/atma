use super::arranged::{ArrangedChunk, ArrangedRegion, ArrangedSection};
use super::error::LinkError;
use super::loose::ChunkId;
use super::place::try_place;
use crate::addr::{Addr, Size};
use rangemap::RangeInclusiveSet;
use std::cmp::Ordering;

//===========================================================================//

/// Represents a data chunk that has been positioned within its memory region.
pub struct PositionedChunk {
    /// The ID for this chunk.
    pub id: ChunkId,
    /// The absolute starting address of this chunk within its address space.
    pub start: Addr,
    /// The offset into the final binary where this chunk's data should be
    /// written, or `None` if this chunk should not be included in the final
    /// binary.
    pub binary_offset: Option<u64>,
}

impl PositionedChunk {
    fn with_section_start(
        chunk: ArrangedChunk,
        section_start: Addr,
        section_binary_offset: Option<u64>,
    ) -> PositionedChunk {
        let chunk_start = section_start + chunk.offset;
        let chunk_binary_offset = section_binary_offset
            .map(|offset| offset + u64::try_from(chunk.offset).unwrap());
        PositionedChunk {
            id: chunk.id,
            start: chunk_start,
            binary_offset: chunk_binary_offset,
        }
    }
}

//===========================================================================//

/// Represents a data section that has been positioned within its memory
/// region.
pub struct PositionedSection {
    /// The chunks in this section.
    pub chunks: Vec<PositionedChunk>,
}

impl PositionedSection {
    fn with_start(
        section: ArrangedSection,
        section_start: Addr,
        section_binary_offset: Option<u64>,
    ) -> PositionedSection {
        PositionedSection {
            chunks: section
                .chunks
                .into_iter()
                .map(|chunk| {
                    PositionedChunk::with_section_start(
                        chunk,
                        section_start,
                        section_binary_offset,
                    )
                })
                .collect(),
        }
    }
}

//===========================================================================//

/// Represents a memory region after its sections have been positioned within
/// its address space.
pub struct PositionedRegion {
    /// The sections in this memory region.
    pub sections: Vec<PositionedSection>,
}

impl PositionedRegion {
    pub fn position(
        regions: Vec<ArrangedRegion>,
        errors: &mut Vec<LinkError>,
    ) -> Vec<PositionedRegion> {
        let mut cumulative_offset: u64 = 0;
        regions
            .into_iter()
            .map(|region| {
                PositionedRegion::position_one(
                    region,
                    &mut cumulative_offset,
                    errors,
                )
            })
            .collect::<Vec<PositionedRegion>>()
    }

    fn position_one(
        region: ArrangedRegion,
        cumulative_offset: &mut u64,
        errors: &mut Vec<LinkError>,
    ) -> PositionedRegion {
        let mut range_set = RangeInclusiveSet::<Addr>::new();
        let mut positioned_sections =
            Vec::<PositionedSection>::with_capacity(region.sections.len());
        // TODO: Sometimes make this None, depending on region config
        let region_binary_offset = Some(*cumulative_offset);
        let mut arranged_sections = region.sections;
        arranged_sections.sort_by(section_ordering);
        for section in arranged_sections {
            if section.size == Size::ZERO {
                errors.push(LinkError::SectionIsEmpty {
                    section_name: section.name,
                });
                continue;
            }
            if let Some(section_range) = try_place(
                &range_set,
                region.range,
                section.size,
                section.start,
                section.align,
                section.within,
            ) {
                let section_offset =
                    section_range.start() - region.range.start();
                let section_binary_offset =
                    region_binary_offset.map(|offset| {
                        offset + u64::try_from(section_offset).unwrap()
                    });
                positioned_sections.push(PositionedSection::with_start(
                    section,
                    section_range.start(),
                    section_binary_offset,
                ));
                range_set.insert(section_range.into());
            } else {
                errors.push(LinkError::SectionCannotBePlaced);
            }
        }
        if region_binary_offset.is_some() {
            // TODO: If this region is not set to fill, then reduce
            // `region_size` to the size actually occupied.
            let region_binary_size = u64::from(region.range.size());
            *cumulative_offset += region_binary_size;
        }
        PositionedRegion { sections: positioned_sections }
    }
}

/// Comparison function for sorting the `ArrangedSection`s in a memory region
/// into the order in which they should be positioned within that region. All
/// sections with an explicit `start` address come first, in address order. Any
/// remaining sections are sorted by their `align` settings, in descending
/// order.
fn section_ordering(lhs: &ArrangedSection, rhs: &ArrangedSection) -> Ordering {
    match (lhs.start, rhs.start) {
        (Some(lhs_start), Some(rhs_start)) => lhs_start.cmp(&rhs_start),
        (Some(_lhs_start), None) => Ordering::Less,
        (None, Some(_rhs_start)) => Ordering::Greater,
        (None, None) => rhs.align.cmp(&lhs.align),
    }
}

//===========================================================================//
