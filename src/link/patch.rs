use super::error::LinkError;
use crate::addr::Addr;
use crate::expr::ExprValue;
use crate::obj::{ObjChunk, ObjExpr, ObjExprOp, ObjFile, ObjPatch, PatchKind};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// An object file that has had all of its link-time patches applied.
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
        let mut symbol_addrs = HashMap::<Rc<str>, Option<Addr>>::new();
        for name in object_file.imports {
            let addr = exported_symbols.get(&name).copied();
            if addr.is_none() {
                errors.push(LinkError::SymbolImportUnresolved {
                    symbol_name: name.clone(),
                });
            }
            symbol_addrs.insert(name, addr);
        }
        for chunk in &object_file.chunks {
            for symbol in chunk.symbols.iter() {
                // TODO: don't use `exported_symbols` for local symbols
                let addr = exported_symbols.get(&symbol.name).copied();
                if addr.is_none() {
                    errors.push(LinkError::SymbolImportUnresolved {
                        symbol_name: symbol.name.clone(),
                    });
                }
                symbol_addrs.insert(symbol.name.clone(), addr);
            }
        }

        let mut patched_chunks = Vec::<PatchedChunk>::new();
        let mut patcher = FilePatcher::new(symbol_addrs, errors);
        for chunk in object_file.chunks {
            patched_chunks.push(patcher.patch_chunk(chunk));
        }
        PatchedFile { chunks: patched_chunks }
    }
}

//===========================================================================//

/// A data chunk of an object file that has had all of its link-time patches
/// applied.
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
    /// For each symbol declared in the object file, local or imported, this
    /// stores the run address of the symbol, or `None` if the symbol couldn't
    /// be resolved.
    symbol_addrs: HashMap<Rc<str>, Option<Addr>>,
    errors: &'a mut Vec<LinkError>,
}

impl<'a> FilePatcher<'a> {
    pub fn new(
        symbol_addrs: HashMap<Rc<str>, Option<Addr>>,
        errors: &'a mut Vec<LinkError>,
    ) -> FilePatcher<'a> {
        FilePatcher { symbol_addrs, errors }
    }

    pub fn patch_chunk(&mut self, chunk: ObjChunk) -> PatchedChunk {
        let mut data = chunk.data;
        for patch in chunk.patches.into_iter() {
            self.apply_patch(patch, &mut data);
        }
        PatchedChunk { data }
    }

    fn apply_patch(&mut self, patch: ObjPatch, data: &mut [u8]) {
        if let Ok(start) = usize::try_from(patch.offset)
            && let Some(end) = start.checked_add(patch.kind.num_bytes())
            && end <= data.len()
        {
            match self.eval_patch_expr(patch.expr) {
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

    /// Evaluates an expression and returns the result value, or `None` if an
    /// error occurred (in which case the error has already been reported when
    /// this returns).
    fn eval_patch_expr(&mut self, expr: ObjExpr) -> Option<ExprValue> {
        let mut value_stack = Vec::<ExprValue>::new();
        for op in expr.ops {
            match op {
                ObjExprOp::LabelAddr => {
                    if let Some(ExprValue::Label(name, offset)) =
                        value_stack.pop()
                        && let Some(opt_addr) =
                            self.symbol_addrs.get(&name).copied()
                    {
                        if let Some(addr) = opt_addr {
                            value_stack.push(ExprValue::Integer(
                                &BigInt::from(addr) + &*offset,
                            ));
                        } else {
                            // This symbol was never resolved.  An error has
                            // already been reported for this, so no need to
                            // report another here.
                            return None;
                        }
                    } else {
                        // The expression either has a type error or references
                        // a symbol that was never declared in this object
                        // file.  That shouldn't happen if unless the object
                        // file was corrupted.
                        self.errors.push(LinkError::MalformedPatchExpression);
                        return None;
                    }
                }
                ObjExprOp::Push(value) => value_stack.push(value),
                other => todo!("{other:?}"),
            }
        }
        if let Some(value) = value_stack.pop()
            && value_stack.is_empty()
        {
            Some(value)
        } else {
            self.errors.push(LinkError::MalformedPatchExpression);
            None
        }
    }
}

//===========================================================================//

fn write_patch_value(
    kind: PatchKind,
    offset: usize,
    value: i64,
    data: &mut [u8],
) {
    debug_assert!(offset + kind.num_bytes() <= data.len());
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
