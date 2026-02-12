use super::config::LinkConfig;
use super::error::LinkError;
use crate::addr::{Addr, Align, Size};
use crate::obj::ObjFile;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// Uniquely identifies one chunk in one of a collection of object files.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct ChunkId {
    /// The index of the object file that contains the chunk.
    pub object_index: usize,
    /// The index of the chunk within its object file.
    pub chunk_index: usize,
}

//===========================================================================//

pub struct LooseChunk {
    /// The ID for this chunk.
    pub id: ChunkId,
    /// The size of the chunk, in bytes.
    pub size: Size,
    /// The required alignment for this chunk's data, within its address space.
    pub align: Align,
    /// If set, then this entire chunk must not cross any alignment boundary of
    /// this size within its address space.
    pub within: Option<Align>,
    /// If set, then any padded portions of the chunk will be filled with this
    /// byte value. Otherwise, they will be filled with this chunk's section's
    /// fill byte.
    pub fill: Option<u8>,
}

//===========================================================================//

pub struct LooseSection {
    /// The name of this section.
    pub name: Rc<str>,
    /// The name of the memory region that this section should be loaded into.
    pub load: Rc<str>,
    /// If set, then the section must start at exactly this address.
    pub start: Option<Addr>,
    /// The required alignment for this section, within its address space.
    pub align: Align,
    /// If set, then this entire section must not cross any alignment boundary
    /// of this size within its address space.
    pub within: Option<Align>,
    /// If set, then any padded portions of the section will be filled with
    /// this byte value. Otherwise, they will be filled with this sections's
    /// memory region's fill byte.
    pub fill: Option<u8>,
    /// The chunks in this section.
    pub chunks: Vec<LooseChunk>,
}

impl LooseSection {
    pub fn collect(
        config: &LinkConfig,
        object_files: &[ObjFile],
        errors: &mut Vec<LinkError>,
    ) -> Vec<LooseSection> {
        let mut loose_sections = Vec::<LooseSection>::new();
        let section_name_to_index: HashMap<Rc<str>, usize> = {
            let mut map = HashMap::new();
            for section in config.sections.iter() {
                map.insert(section.name.clone(), loose_sections.len());
                loose_sections.push(LooseSection {
                    name: section.name.clone(),
                    load: section.load.clone(),
                    start: section.start,
                    align: section.align,
                    within: section.within,
                    fill: section.fill,
                    chunks: Vec::new(),
                })
            }
            map
        };
        for (object_index, object_file) in object_files.iter().enumerate() {
            for (chunk_index, chunk) in object_file.chunks.iter().enumerate() {
                let section_index =
                    match section_name_to_index.get(&chunk.section_name) {
                        Some(index) => *index,
                        None => {
                            errors.push(LinkError::ChunkSectionDoesNotExist {
                                section_name: chunk.section_name.clone(),
                            });
                            continue;
                        }
                    };
                if let Ok(data_size) = Size::try_from(chunk.data.len())
                    && data_size <= chunk.size
                {
                    let loose_chunk = LooseChunk {
                        id: ChunkId { object_index, chunk_index },
                        size: chunk.size,
                        align: chunk.align,
                        within: chunk.within,
                        fill: chunk.fill,
                    };
                    loose_sections[section_index].chunks.push(loose_chunk);
                } else {
                    errors.push(LinkError::ChunkDataLargerThanSize {
                        chunk_data_len: chunk.data.len(),
                        chunk_size: chunk.size,
                    });
                }
            }
        }
        loose_sections
    }
}

//===========================================================================//
