use super::arranged::{ArrangedChunk, ArrangedRegion, ArrangedSection};
use super::config::LinkConfig;
use super::error::LinkError;
use super::loose::ChunkId;
use super::place::try_place;
use crate::addr::{Addr, Size};
use crate::obj::ObjFile;
use rangemap::RangeInclusiveSet;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// Represents a data chunk that has been positioned within its memory region.
pub struct PositionedChunk {
    /// The ID for this chunk.
    pub id: ChunkId,
    /// The offset into the final binary where this chunk's data should be
    /// written.
    pub binary_offset: u64,
    /// The size of this chunk in the final binary, in bytes.
    pub binary_size: u64,
    /// The absolute starting address of this chunk within its address space.
    pub start: Addr,
    /// Any padded portions of this chunk will be filled with this byte value.
    pub fill: u8,
}

impl PositionedChunk {
    fn with_section_start(
        chunk: ArrangedChunk,
        section_start: Addr,
        section_binary_offset: Option<u64>,
        section_fill: u8,
    ) -> PositionedChunk {
        let chunk_start = section_start + chunk.offset;
        let (binary_offset, binary_size) = match section_binary_offset {
            Some(offset) => {
                (offset + u64::from(chunk.offset), chunk.size.into_u64())
            }
            None => (0, 0),
        };
        PositionedChunk {
            id: chunk.id,
            binary_offset,
            binary_size,
            start: chunk_start,
            fill: chunk.fill.unwrap_or(section_fill),
        }
    }
}

//===========================================================================//

/// Represents a data section that has been positioned within its memory
/// region.
pub struct PositionedSection {
    /// The offset into the final binary where this section's data should be
    /// written.
    pub binary_offset: u64,
    /// The size of this section in the final binary, in bytes, or zero if this
    /// region should not appear in the final binary.
    pub binary_size: u64,
    /// The chunks in this section.
    pub chunks: Vec<PositionedChunk>,
    /// Any padded portions of this section will be filled with this byte
    /// value.
    pub fill: u8,
}

impl PositionedSection {
    fn with_start(
        section: ArrangedSection,
        section_start: Addr,
        section_binary_offset: Option<u64>,
        region_fill: u8,
    ) -> PositionedSection {
        let (binary_offset, binary_size) = match section_binary_offset {
            Some(offset) => (offset, section.size.into_u64()),
            None => (0, 0),
        };
        let section_fill = section.fill.unwrap_or(region_fill);
        PositionedSection {
            binary_offset,
            binary_size,
            chunks: section
                .chunks
                .into_iter()
                .map(|chunk| {
                    PositionedChunk::with_section_start(
                        chunk,
                        section_start,
                        section_binary_offset,
                        section_fill,
                    )
                })
                .collect(),
            fill: section_fill,
        }
    }
}

//===========================================================================//

/// Represents a memory region after its sections have been positioned within
/// its address space.
pub struct PositionedRegion {
    /// The size of this memory region in the final binary, in bytes, or zero
    /// if this region should not appear in the final binary.
    pub binary_size: u64,
    /// The sections in this memory region.
    pub sections: Vec<PositionedSection>,
    /// Any padded portions of this memory region will be filled with this byte
    /// value.
    pub fill: u8,
}

impl PositionedRegion {
    fn position(
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
                let section_binary_offset = region_binary_offset
                    .map(|offset| offset + u64::from(section_offset));
                positioned_sections.push(PositionedSection::with_start(
                    section,
                    section_range.start(),
                    section_binary_offset,
                    region.fill,
                ));
                range_set.insert(section_range.into());
            } else {
                errors.push(LinkError::SectionCannotBePlaced {
                    section_name: section.name,
                });
            }
        }
        // If this memory region is to be included in the final binary, then
        // update `cumulative_offset` appropriately.
        let region_binary_size = if region_binary_offset.is_some() {
            // TODO: If this region is not set to fill, then reduce
            // `region_range` to the range actually occupied.
            let region_range = region.range;
            if let Ok(size) = u64::try_from(region_range.size())
                && let Some(sum) = cumulative_offset.checked_add(size)
            {
                *cumulative_offset = sum;
                size
            } else {
                errors.push(LinkError::BinaryTooLarge {
                    region_name: region.name.clone(),
                });
                0
            }
        } else {
            0
        };
        PositionedRegion {
            binary_size: region_binary_size,
            sections: positioned_sections,
            fill: region.fill,
        }
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

/// Positioning information for all regions of a complete binary.
pub struct PositionedBinary {
    /// The memory regions for this binary.
    pub regions: Vec<PositionedRegion>,
    /// The address of each exported symbol across the binary.
    pub exported_symbols: HashMap<Rc<str>, Addr>,
}

impl PositionedBinary {
    pub fn position(
        config: &LinkConfig,
        object_files: &[ObjFile],
    ) -> Result<PositionedBinary, Vec<LinkError>> {
        let mut errors = Vec::<LinkError>::new();
        let mut cumulative_offset: u64 = 0;
        let regions =
            ArrangedRegion::collect(config, object_files, &mut errors);
        let positioned_regions = regions
            .into_iter()
            .map(|region| {
                PositionedRegion::position(
                    region,
                    &mut cumulative_offset,
                    &mut errors,
                )
            })
            .collect::<Vec<PositionedRegion>>();

        let mut chunk_starts = HashMap::<ChunkId, Addr>::new();
        for region in &positioned_regions {
            for section in &region.sections {
                for chunk in &section.chunks {
                    chunk_starts.insert(chunk.id, chunk.start);
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        let mut exported_symbols = HashMap::<Rc<str>, Addr>::new();
        for (object_index, object_file) in object_files.iter().enumerate() {
            for (chunk_index, chunk) in object_file.chunks.iter().enumerate() {
                let chunk_id = ChunkId { object_index, chunk_index };
                let chunk_start = chunk_starts[&chunk_id];
                for symbol in chunk.symbols.iter() {
                    if !symbol.exported {
                        continue;
                    }
                    let symbol_addr = chunk_start + symbol.offset;
                    let collision = exported_symbols
                        .insert(symbol.name.clone(), symbol_addr);
                    if collision.is_some() {
                        errors.push(LinkError::SymbolExportCollision {
                            symbol_name: symbol.name.clone(),
                        });
                    }
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(PositionedBinary { regions: positioned_regions, exported_symbols })
    }
}

//===========================================================================//
