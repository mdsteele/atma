use super::error::LinkError;
use super::loose::ChunkId;
use super::positioned::PositionedChunk;
use crate::bus::Addr;
use crate::expr::ExprValue;
use crate::obj::{ObjChunk, ObjExpr, ObjFile, ObjPatch, PatchKind};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// Represents one data chunk of an object file, after it has had its patches
/// applied.
pub struct PatchedChunk {
    /// The offset into the final binary where this chunk's data should be
    /// written.
    pub binary_offset: u64,
    /// The patched chunk data.
    pub data: Box<[u8]>,
}

impl PatchedChunk {
    pub(crate) fn patch_all(
        object_files: Vec<ObjFile>,
        chunk_positions: &HashMap<ChunkId, PositionedChunk>,
        exported_symbols: &HashMap<Rc<str>, Addr>,
    ) -> Result<Vec<PatchedChunk>, Vec<LinkError>> {
        let mut errors = Vec::<LinkError>::new();
        let mut patched_chunks = Vec::<PatchedChunk>::new();
        for (object_index, file) in object_files.into_iter().enumerate() {
            let mut patcher = FilePatcher::new(exported_symbols, &mut errors);
            for (chunk_index, chunk) in file.chunks.into_iter().enumerate() {
                let chunk_id = ChunkId { object_index, chunk_index };
                let offset = chunk_positions[&chunk_id].binary_offset;
                if let Some(patched) = patcher.patch_chunk(chunk, offset) {
                    patched_chunks.push(patched);
                }
            }
        }
        if errors.is_empty() {
            patched_chunks.sort_by_key(|chunk| chunk.binary_offset);
            Ok(patched_chunks)
        } else {
            Err(errors)
        }
    }
}

//===========================================================================//

struct FilePatcher<'a> {
    _exported_symbols: &'a HashMap<Rc<str>, Addr>,
    errors: &'a mut Vec<LinkError>,
}

impl<'a> FilePatcher<'a> {
    pub fn new(
        exported_symbols: &'a HashMap<Rc<str>, Addr>,
        errors: &'a mut Vec<LinkError>,
    ) -> FilePatcher<'a> {
        FilePatcher { _exported_symbols: exported_symbols, errors }
    }

    pub fn patch_chunk(
        &mut self,
        chunk: ObjChunk,
        binary_offset: Option<u64>,
    ) -> Option<PatchedChunk> {
        let mut data = chunk.data;
        for patch in chunk.patches.iter() {
            self.apply_patch(patch, &mut data);
        }
        binary_offset.map(|off| PatchedChunk { data, binary_offset: off })
    }

    fn apply_patch(&mut self, patch: &ObjPatch, data: &mut [u8]) {
        if (patch.offset as usize) + patch.kind.num_bytes() >= data.len() {
            self.errors.push(LinkError::PatchOffsetOutOfRange);
            return;
        }
        match self.eval_expr(&patch.expr) {
            None => {} // evaluation failed with errors
            Some(ExprValue::Integer(bigint)) => {
                match patch.kind.value_in_range(&bigint) {
                    Ok(int) => {
                        write_patch_value(patch.kind, patch.offset, int, data)
                    }
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

//===========================================================================//

fn write_patch_value(
    kind: PatchKind,
    offset: u32,
    value: i64,
    data: &mut [u8],
) {
    let offset = offset as usize;
    debug_assert!(offset + kind.num_bytes() < data.len());
    match kind {
        PatchKind::U8 => {
            data[offset] = value as u8;
        }
        PatchKind::U16le => {
            data[offset] = value as u8;
            data[offset + 1] = (value >> 8) as u8;
        }
        PatchKind::U24le => {
            data[offset] = value as u8;
            data[offset + 1] = (value >> 8) as u8;
            data[offset + 2] = (value >> 16) as u8;
        }
    }
}

//===========================================================================//
