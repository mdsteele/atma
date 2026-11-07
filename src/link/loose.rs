use super::config::{LinkConfig, RegionKind};
use super::error::{LinkError, LinkResult};
use super::types::ChunkId;
use crate::addr::{Addr, Align, Size};
use crate::error::Errs;
use crate::obj::{ObjFile, ObjSrcLoc};
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
    /// The linker config source code location where the section was
    /// declared.
    pub name_loc: ObjSrcLoc,
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
    /// If set, then this section will mirror the data of the named section.
    pub mirror: Option<(ObjSrcLoc, Rc<str>)>,
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
        let mut loose_sections =
            errs.with(Self::create_empty_loose_sections(config));
        errs.also(Self::distribute_chunks_into_sections(
            object_files,
            &mut loose_sections,
        ));
        (loose_sections, errs)
    }

    /// Returns the name of each BSS memory region in the config.  Reports
    /// errors for any BSS regions that have a fill byte set.
    fn bss_region_names(
        config: &LinkConfig,
    ) -> (HashSet<Rc<str>>, Errs<LinkError>) {
        let mut errs = Errs::<LinkError>::new();
        let mut bss_region_names = HashSet::<Rc<str>>::new();
        for region in &config.regions {
            if let RegionKind::Bss = region.kind {
                bss_region_names.insert(region.name.clone());
                if region.fill.is_some() {
                    errs.push(LinkError::FillByteOnBssRegion);
                }
            }
        }
        (bss_region_names, errs)
    }

    /// Creates a list of `LooseSection`s from the config, each initially with
    /// an empty list of chunks.  Reports errors for any BSS sections that have
    /// a fill byte set or that mirror another section.
    fn create_empty_loose_sections(
        config: &LinkConfig,
    ) -> (Vec<LooseSection>, Errs<LinkError>) {
        let mut errs = Errs::<LinkError>::new();
        let bss_region_names = errs.with(Self::bss_region_names(config));
        let mut loose_sections = Vec::<LooseSection>::new();
        for section in config.sections.iter() {
            let is_bss = bss_region_names.contains(&section.region);
            if is_bss {
                if section.fill.is_some() {
                    errs.push(LinkError::FillByteOnBssSection);
                }
                if section.mirror.is_some() {
                    errs.push(LinkError::MirrorBssSection);
                }
            }
            loose_sections.push(LooseSection {
                name: section.name.clone(),
                name_loc: section.name_loc.clone(),
                region: section.region.clone(),
                start: section.start,
                size: section.size,
                align: section.align,
                within: section.within,
                fill: section.fill,
                mirror: section.mirror.clone(),
                chunks: Vec::new(),
                is_bss,
            });
        }
        (loose_sections, errs)
    }

    /// For each chunk in each object file, adds a corresponding `LooseChunk`
    /// to the appropriate `LooseSection`.
    pub(super) fn distribute_chunks_into_sections(
        object_files: &[ObjFile],
        loose_sections: &mut [LooseSection],
    ) -> LinkResult<()> {
        let mut errs = Errs::<LinkError>::new();
        let section_name_to_index = loose_sections
            .iter()
            .enumerate()
            .map(|(index, section)| (section.name.clone(), index))
            .collect::<HashMap<Rc<str>, usize>>();
        for (object_index, object_file) in object_files.iter().enumerate() {
            for (chunk_index, chunk) in object_file.chunks.iter().enumerate() {
                let Some(&section_index) =
                    section_name_to_index.get(&chunk.section_name)
                else {
                    errs.push(LinkError::ChunkSectionDoesNotExist {
                        section_name: chunk.section_name.clone(),
                        section_name_loc: chunk.section_name_loc.clone(),
                    });
                    continue;
                };
                let section = &mut loose_sections[section_index];
                if let Some((mirror_loc, mirror_name)) = &section.mirror {
                    errs.push(LinkError::ChunkInMirrorSection {
                        section_name: chunk.section_name.clone(),
                        mirror_name: mirror_name.clone(),
                        chunk_section_name_loc: chunk.section_name_loc.clone(),
                        config_section_name_loc: section.name_loc.clone(),
                        config_mirror_name_loc: mirror_loc.clone(),
                    });
                    continue;
                }
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
        errs.result()
    }
}

//===========================================================================//
