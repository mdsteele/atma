use super::arranged::{ArrangedChunk, ArrangedRegion, ArrangedSection};
use super::config::LinkConfig;
use super::error::{LinkError, LinkResult};
use super::place::try_place;
use super::types::{AbsoluteLabel, ChunkId, ChunkMetadata};
use crate::addr::{Addr, Size};
use crate::error::Errs;
use crate::obj::ObjFile;
use rangemap::RangeInclusiveSet;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// Represents a data chunk that has been positioned within its memory region.
pub(super) struct PositionedChunk {
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
pub(super) struct PositionedSection {
    /// The offset into the final binary where this section's data should be
    /// written.
    pub binary_offset: u64,
    /// The size of this section in the final binary, in bytes, or zero if this
    /// region should not appear in the final binary.
    pub binary_size: u64,
    /// The chunks in this section, sorted by their offsets within the binary.
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
pub(super) struct PositionedRegion {
    /// The name of the address space that this memory region exists in.
    pub space: Rc<str>,
    /// The size of this memory region in the final binary, in bytes, or zero
    /// if this region should not appear in the final binary.
    pub binary_size: u64,
    /// The sections in this memory region, sorted by their offsets within the
    /// binary.
    pub sections: Vec<PositionedSection>,
    /// Any padded portions of this memory region will be filled with this byte
    /// value.
    pub fill: u8,
}

impl PositionedRegion {
    fn position_all(
        config: &LinkConfig,
        object_files: &[ObjFile],
    ) -> LinkResult<Vec<PositionedRegion>> {
        let mut errs = Errs::<LinkError>::new();
        let mut cumulative_offset: u64 = 0;
        let regions = errs.with(ArrangedRegion::collect(config, object_files));
        let positioned_regions = regions
            .into_iter()
            .filter_map(|region| {
                errs.ok(PositionedRegion::position(
                    region,
                    &mut cumulative_offset,
                ))
            })
            .collect::<Vec<PositionedRegion>>();
        errs.result()?;
        Ok(positioned_regions)
    }

    fn position(
        region: ArrangedRegion,
        cumulative_offset: &mut u64,
    ) -> LinkResult<PositionedRegion> {
        let mut errs = Errs::<LinkError>::new();
        let mut range_set = RangeInclusiveSet::<Addr>::new();
        let mut positioned_sections =
            Vec::<PositionedSection>::with_capacity(region.sections.len());
        // TODO: Sometimes make this None, depending on region config
        let region_binary_offset = Some(*cumulative_offset);
        let mut arranged_sections = region.sections;
        arranged_sections.sort_by(section_ordering);
        for section in arranged_sections {
            if section.size == Size::ZERO {
                errs.push(LinkError::SectionIsEmpty {
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
                errs.push(LinkError::SectionCannotBePlaced {
                    section_name: section.name,
                });
            }
        }
        positioned_sections.sort_by_key(|section| section.binary_offset);
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
                errs.push(LinkError::BinaryTooLarge {
                    region_name: region.name.clone(),
                });
                0
            }
        } else {
            0
        };
        errs.result()?;
        Ok(PositionedRegion {
            space: region.space,
            binary_size: region_binary_size,
            sections: positioned_sections,
            fill: region.fill,
        })
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
pub(super) struct PositionedBinary {
    /// The memory regions for this binary.
    pub regions: Vec<PositionedRegion>,
    /// For each object file, metadata about each chunk in that object file.
    pub file_chunk_metadata: Vec<Vec<ChunkMetadata>>,
    /// The address of each exported symbol across the binary.
    pub exported_symbols: HashMap<Rc<str>, AbsoluteLabel>,
}

impl PositionedBinary {
    pub(super) fn position(
        config: &LinkConfig,
        object_files: &[ObjFile],
    ) -> LinkResult<PositionedBinary> {
        let positioned_regions =
            PositionedRegion::position_all(config, object_files)?;
        let chunk_metadata =
            PositionedBinary::make_chunk_metadata(&positioned_regions);

        let mut exported_symbols = HashMap::<Rc<str>, AbsoluteLabel>::new();
        for export in &config.exports {
            exported_symbols.insert(
                export.name.clone(),
                AbsoluteLabel {
                    space: export.space.clone(),
                    address: export.address,
                },
            );
        }

        let mut errs = Errs::<LinkError>::new();
        for (object_index, object_file) in object_files.iter().enumerate() {
            for (chunk_index, chunk) in object_file.chunks.iter().enumerate() {
                let chunk_id = ChunkId { object_index, chunk_index };
                let chunk_start = chunk_metadata[&chunk_id].start.clone();
                for symbol in chunk.symbols.iter() {
                    if !symbol.exported {
                        continue;
                    }
                    let symbol_addr = chunk_start.address + symbol.offset;
                    let collision = exported_symbols.insert(
                        symbol.name.clone(),
                        AbsoluteLabel {
                            space: chunk_start.space.clone(),
                            address: symbol_addr,
                        },
                    );
                    if collision.is_some() {
                        errs.push(LinkError::SymbolExportCollision {
                            symbol_name: symbol.name.clone(),
                        });
                    }
                }
            }
        }
        errs.result()?;

        let file_chunk_metadata = object_files
            .iter()
            .enumerate()
            .map(|(object_index, object_file)| {
                (0..object_file.chunks.len())
                    .map(|chunk_index| {
                        chunk_metadata[&ChunkId { object_index, chunk_index }]
                            .clone()
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        Ok(PositionedBinary {
            regions: positioned_regions,
            file_chunk_metadata,
            exported_symbols,
        })
    }

    fn make_chunk_metadata(
        positioned_regions: &[PositionedRegion],
    ) -> HashMap<ChunkId, ChunkMetadata> {
        let mut chunk_metadata = HashMap::<ChunkId, ChunkMetadata>::new();
        for region in positioned_regions {
            for section in &region.sections {
                for chunk in &section.chunks {
                    let metadata = ChunkMetadata {
                        start: AbsoluteLabel {
                            space: region.space.clone(),
                            address: chunk.start,
                        },
                        fill: chunk.fill,
                    };
                    chunk_metadata.insert(chunk.id, metadata);
                }
            }
        }
        chunk_metadata
    }
}

//===========================================================================//
