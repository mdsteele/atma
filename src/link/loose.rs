use super::config::LinkConfig;
use super::error::LinkError;
use super::types::ChunkId;
use crate::addr::{Addr, Align, Size};
use crate::error::Errs;
use crate::obj::ObjFile;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

//===========================================================================//

pub(super) struct LooseChunk {
    /// The ID for this chunk.
    pub id: ChunkId,
    /// The size of the chunk, in bytes.
    pub size: Size,
    /// If set, then the chunk must start at this exact address within its
    /// address space.
    pub start: Option<Addr>,
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

pub(super) struct LooseSection {
    /// The name of this section.
    pub name: Rc<str>,
    /// The name of the memory region that this section should be placed in.
    pub region: Rc<str>,
    /// If set, then the section must start at exactly this address.
    pub start: Option<Addr>,
    /// If set, then the section will have exactly this size.
    pub size: Option<Size>,
    /// The required alignment for this section, within its address space.
    pub align: Align,
    /// If set, then this entire section must not cross any alignment boundary
    /// of this size within its address space.
    pub within: Option<Align>,
    /// If set, then any padded portions of the section will be filled with
    /// this byte value. Otherwise, they will be filled with this sections's
    /// memory region's fill byte.
    pub fill: Option<u8>,
    /// All the chunks in this section, listed in the same order in which they
    /// appear across the object files.
    pub chunks: Vec<LooseChunk>,
    /// If true, this section belongs to a BSS region, and may only contain
    /// padding, not any data.
    pub is_bss: bool,
}

impl LooseSection {
    pub(super) fn collect(
        config: &LinkConfig,
        object_files: &[ObjFile],
    ) -> (Vec<LooseSection>, Errs<LinkError>) {
        let mut errs = Errs::<LinkError>::new();
        for region in &config.bss {
            if region.fill.is_some() {
                errs.push(LinkError::FillByteOnBssRegion);
            }
        }
        let bss_region_names = config
            .bss
            .iter()
            .map(|region| region.name.clone())
            .collect::<HashSet<Rc<str>>>();
        let mut loose_sections = Vec::<LooseSection>::new();
        let section_name_to_index: HashMap<Rc<str>, usize> = {
            let mut map = HashMap::new();
            for section in config.sections.iter() {
                map.insert(section.name.clone(), loose_sections.len());
                let is_bss = bss_region_names.contains(&section.region);
                if is_bss && section.fill.is_some() {
                    errs.push(LinkError::FillByteOnBssSection);
                }
                loose_sections.push(LooseSection {
                    name: section.name.clone(),
                    region: section.region.clone(),
                    start: section.start,
                    size: section.size,
                    align: section.align,
                    within: section.within,
                    fill: section.fill,
                    chunks: Vec::new(),
                    is_bss,
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
                            errs.push(LinkError::ChunkSectionDoesNotExist {
                                section_name: chunk.section_name.clone(),
                            });
                            continue;
                        }
                    };
                let section = &mut loose_sections[section_index];
                if section.is_bss {
                    if chunk.fill.is_some() {
                        errs.push(LinkError::FillByteOnBssChunk);
                    }
                    if !chunk.data.is_empty() {
                        errs.push(LinkError::DataInBssChunk);
                    }
                }
                if let Ok(data_size) = Size::try_from(chunk.data.len())
                    && data_size <= chunk.size
                {
                    let loose_chunk = LooseChunk {
                        id: ChunkId { object_index, chunk_index },
                        size: chunk.size,
                        start: chunk.start,
                        align: chunk.align,
                        within: chunk.within,
                        fill: chunk.fill,
                    };
                    section.chunks.push(loose_chunk);
                } else {
                    errs.push(LinkError::ChunkDataLargerThanSize {
                        chunk_data_len: chunk.data.len(),
                        chunk_size: chunk.size,
                    });
                }
            }
        }
        (loose_sections, errs)
    }
}

//===========================================================================//
