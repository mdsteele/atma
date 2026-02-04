//! Facilities for linking objects files together into a binary.

mod arranged;
mod config;
mod error;
mod expr;
mod loose;
mod patch;
mod place;
mod positioned;

pub use config::{AddrspaceConfig, LinkConfig, MemoryConfig, SectionConfig};
pub use error::LinkError;
pub use patch::PatchedChunk;

use crate::addr::Addr;
use crate::obj::ObjFile;
use arranged::{ArrangedRegion, ArrangedSection};
use loose::{ChunkId, LooseSection};
use positioned::{PositionedChunk, PositionedRegion};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

fn position_all_regions(
    config: &LinkConfig,
    object_files: &[ObjFile],
) -> Result<Vec<PositionedRegion>, Vec<LinkError>> {
    let mut errors = Vec::<LinkError>::new();
    let loose_sections =
        LooseSection::collect(config, object_files, &mut errors);
    let arranged_sections =
        ArrangedSection::arrange(loose_sections, &mut errors);
    let arranged_regions =
        ArrangedRegion::collect(config, arranged_sections, &mut errors);
    let positioned_regions =
        PositionedRegion::position(arranged_regions, &mut errors);
    if errors.is_empty() { Ok(positioned_regions) } else { Err(errors) }
}

fn get_chunk_positions(
    positioned_regions: Vec<PositionedRegion>,
) -> HashMap<ChunkId, PositionedChunk> {
    let mut chunk_positions = HashMap::<ChunkId, PositionedChunk>::new();
    for region in positioned_regions {
        for section in region.sections {
            for chunk in section.chunks {
                chunk_positions.insert(chunk.id, chunk);
            }
        }
    }
    chunk_positions
}

fn export_all_symbols(
    object_files: &[ObjFile],
    chunk_positions: &HashMap<ChunkId, PositionedChunk>,
) -> Result<HashMap<Rc<str>, Addr>, Vec<LinkError>> {
    let mut errors = Vec::<LinkError>::new();
    let mut exported_symbols = HashMap::<Rc<str>, Addr>::new();
    for (object_index, object_file) in object_files.iter().enumerate() {
        for (chunk_index, chunk) in object_file.chunks.iter().enumerate() {
            let chunk_id = ChunkId { object_index, chunk_index };
            let chunk_start = chunk_positions[&chunk_id].start;
            for symbol in chunk.symbols.iter() {
                if !symbol.exported {
                    continue;
                }
                let symbol_addr = chunk_start + symbol.offset;
                let collision =
                    exported_symbols.insert(symbol.name.clone(), symbol_addr);
                if collision.is_some() {
                    errors.push(LinkError::SymbolExportCollision {
                        symbol_name: symbol.name.clone(),
                    });
                }
            }
        }
    }
    if errors.is_empty() { Ok(exported_symbols) } else { Err(errors) }
}

/// Given a linker config and a set of object files, patches all chunks and
/// returns them in the order in which they appear in the final binary.
pub fn link_objects(
    config: &LinkConfig,
    object_files: Vec<ObjFile>,
) -> Result<Vec<PatchedChunk>, Vec<LinkError>> {
    let positioned_regions = position_all_regions(config, &object_files)?;
    let chunk_positions = get_chunk_positions(positioned_regions);
    let exported_symbols =
        export_all_symbols(&object_files, &chunk_positions)?;
    PatchedChunk::patch_all(object_files, &chunk_positions, &exported_symbols)
}

//===========================================================================//
