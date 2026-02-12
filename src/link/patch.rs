use super::error::LinkError;
use crate::addr::Addr;
use crate::expr::ExprValue;
use crate::obj::{ObjChunk, ObjExpr, ObjFile, ObjPatch, PatchKind};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub struct PatchedFile {
    /// The patched chunks from this object file.
    pub chunks: Vec<PatchedChunk>,
}

impl PatchedFile {
    pub(crate) fn patch_all(
        object_files: Vec<ObjFile>,
        exported_symbols: &HashMap<Rc<str>, Addr>,
    ) -> Result<Vec<PatchedFile>, Vec<LinkError>> {
        let mut errors = Vec::<LinkError>::new();
        let patched_files = object_files
            .into_iter()
            .map(|file| {
                PatchedFile::patch(file, exported_symbols, &mut errors)
            })
            .collect::<Vec<PatchedFile>>();
        if errors.is_empty() { Ok(patched_files) } else { Err(errors) }
    }

    fn patch(
        object_file: ObjFile,
        exported_symbols: &HashMap<Rc<str>, Addr>,
        errors: &mut Vec<LinkError>,
    ) -> PatchedFile {
        let mut patched_chunks = Vec::<PatchedChunk>::new();
        let mut patcher = FilePatcher::new(exported_symbols, errors);
        for chunk in object_file.chunks {
            patched_chunks.push(patcher.patch_chunk(chunk));
        }
        PatchedFile { chunks: patched_chunks }
    }
}

//===========================================================================//

pub struct PatchedChunk {
    /// The patched chunk data.
    pub data: Box<[u8]>,
}

impl PatchedChunk {
    pub fn take_data(&mut self) -> Box<[u8]> {
        std::mem::take(&mut self.data)
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

    pub fn patch_chunk(&mut self, chunk: ObjChunk) -> PatchedChunk {
        let mut data = chunk.data;
        for patch in chunk.patches.iter() {
            self.apply_patch(patch, &mut data);
        }
        PatchedChunk { data }
    }

    fn apply_patch(&mut self, patch: &ObjPatch, data: &mut [u8]) {
        if let Ok(start) = usize::try_from(patch.offset)
            && let Some(end) = start.checked_add(patch.kind.num_bytes())
            && end <= data.len()
        {
            match self.eval_expr(&patch.expr) {
                None => {} // evaluation failed with errors
                Some(ExprValue::Integer(bigint)) => {
                    match patch.kind.value_in_range(&bigint) {
                        Ok(int) => {
                            write_patch_value(patch.kind, start, int, data)
                        }
                        Err(_range) => {
                            self.errors.push(
                                LinkError::PatchValueOutOfRange {
                                    kind: patch.kind,
                                    value: bigint,
                                },
                            );
                        }
                    }
                }
                Some(_value) => {
                    self.errors.push(LinkError::PatchValueWrongType);
                }
            }
        } else {
            self.errors.push(LinkError::PatchOffsetOutOfRange);
        }
    }

    fn eval_expr(&mut self, expr: &ObjExpr) -> Option<ExprValue> {
        expr.static_value().cloned() // TODO
    }
}

//===========================================================================//

fn write_patch_value(
    kind: PatchKind,
    offset: usize,
    value: i64,
    data: &mut [u8],
) {
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
