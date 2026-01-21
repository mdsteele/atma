use super::arrange::{PositionedChunk, PositionedMemory, PositionedSection};
use super::error::LinkError;
use crate::expr::ExprValue;
use crate::obj::{ObjExpr, ObjPatch, PatchKind};

//===========================================================================//

/// Given a list of all memory regions with their sections positioned within
/// them, produces a final linked binary blob.
pub fn link_positioned_memory(
    regions: &[PositionedMemory],
) -> Result<Vec<u8>, Vec<LinkError>> {
    LinkPatcher::new().link(regions)
}

//===========================================================================//

struct LinkPatcher {
    errors: Vec<LinkError>,
}

impl LinkPatcher {
    pub fn new() -> LinkPatcher {
        LinkPatcher { errors: Vec::new() }
    }

    pub fn link(
        mut self,
        regions: &[PositionedMemory],
    ) -> Result<Vec<u8>, Vec<LinkError>> {
        let total_size = regions.iter().map(|r| r.size as usize).sum();
        let mut binary = vec![0u8; total_size];
        let mut region_start: usize = 0;
        for region in regions {
            let region_size = region.size as usize;
            let region_end = region_start + region_size;
            let region_binary = &mut binary[region_start..region_end];
            self.visit_region(region, region_binary);
            region_start += region_size;
        }
        debug_assert_eq!(region_start, total_size);
        debug_assert_eq!(binary.len(), total_size);
        if self.errors.is_empty() { Ok(binary) } else { Err(self.errors) }
    }

    fn visit_region(
        &mut self,
        region: &PositionedMemory,
        region_binary: &mut [u8],
    ) {
        debug_assert_eq!(region_binary.len(), region.size as usize);
        for section in region.sections.iter() {
            let section_start = (section.start - region.start) as usize;
            let section_size = section.size as usize;
            let section_end = section_start + section_size;
            let section_binary =
                &mut region_binary[section_start..section_end];
            self.visit_section(section, section_binary);
        }
    }

    fn visit_section(
        &mut self,
        section: &PositionedSection,
        section_binary: &mut [u8],
    ) {
        debug_assert_eq!(section_binary.len(), section.size as usize);
        for chunk in section.chunks.iter() {
            let chunk_start = (chunk.start - section.start) as usize;
            let chunk_size = chunk.data.len();
            let chunk_end = chunk_start + chunk_size;
            let chunk_binary = &mut section_binary[chunk_start..chunk_end];
            self.visit_chunk(chunk, chunk_binary);
        }
    }

    fn visit_chunk(
        &mut self,
        chunk: &PositionedChunk,
        chunk_binary: &mut [u8],
    ) {
        debug_assert_eq!(chunk_binary.len(), chunk.data.len());
        chunk_binary.copy_from_slice(&chunk.data);
        for patch in chunk.patches.iter() {
            self.apply_patch(patch, chunk_binary);
        }
    }

    fn apply_patch(&mut self, patch: &ObjPatch, chunk_binary: &mut [u8]) {
        if (patch.offset as usize) + patch.kind.num_bytes()
            >= chunk_binary.len()
        {
            self.errors.push(LinkError::PatchOffsetOutOfRange);
            return;
        }
        match self.eval_expr(&patch.expr) {
            None => {} // evaluation failed with errors
            Some(ExprValue::Integer(bigint)) => {
                match patch.kind.value_in_range(&bigint) {
                    Ok(int) => write_patch_value(
                        patch.kind,
                        patch.offset,
                        int,
                        chunk_binary,
                    ),
                    Err(_range) => {
                        self.errors.push(LinkError::PatchValueOutOfRange {
                            kind: patch.kind,
                            value: bigint,
                        });
                    }
                }
            }
            Some(_value) => {
                self.errors.push(LinkError::PatchValueWrongType);
            }
        }
    }

    fn eval_expr(&mut self, expr: &ObjExpr) -> Option<ExprValue> {
        expr.static_value().cloned() // TODO
    }
}

fn write_patch_value(
    kind: PatchKind,
    offset: u32,
    value: i64,
    chunk_binary: &mut [u8],
) {
    let offset = offset as usize;
    debug_assert!(offset + kind.num_bytes() < chunk_binary.len());
    match kind {
        PatchKind::U8 => {
            chunk_binary[offset] = value as u8;
        }
        PatchKind::U16le => {
            chunk_binary[offset] = value as u8;
            chunk_binary[offset + 1] = (value >> 8) as u8;
        }
        PatchKind::U24le => {
            chunk_binary[offset] = value as u8;
            chunk_binary[offset + 1] = (value >> 8) as u8;
            chunk_binary[offset + 2] = (value >> 16) as u8;
        }
    }
}

//===========================================================================//
