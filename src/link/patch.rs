use super::error::LinkError;
use super::types::AbsoluteLabel;
use crate::addr::Addr;
use crate::expr::{ExprLabel, ExprValue};
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
    pub(super) fn patch_all(
        object_files: Vec<ObjFile>,
        file_chunk_starts: &[Vec<AbsoluteLabel>],
        exported_symbols: &HashMap<Rc<str>, AbsoluteLabel>,
    ) -> Result<Vec<PatchedFile>, Vec<LinkError>> {
        debug_assert_eq!(object_files.len(), file_chunk_starts.len());
        let mut errors = Vec::<LinkError>::new();
        let patched_files = object_files
            .into_iter()
            .zip(file_chunk_starts)
            .map(|(file, chunk_starts)| {
                PatchedFile::patch(
                    file,
                    chunk_starts,
                    exported_symbols,
                    &mut errors,
                )
            })
            .collect::<Vec<PatchedFile>>();
        if errors.is_empty() { Ok(patched_files) } else { Err(errors) }
    }

    fn patch(
        object_file: ObjFile,
        chunk_starts: &[AbsoluteLabel],
        exported_symbols: &HashMap<Rc<str>, AbsoluteLabel>,
        errors: &mut Vec<LinkError>,
    ) -> PatchedFile {
        let mut symbol_addrs =
            HashMap::<Rc<str>, Option<AbsoluteLabel>>::new();
        for name in object_file.imports {
            let addr = exported_symbols.get(&name).cloned();
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
                let addr = exported_symbols.get(&symbol.name).cloned();
                if addr.is_none() {
                    errors.push(LinkError::SymbolImportUnresolved {
                        symbol_name: symbol.name.clone(),
                    });
                }
                symbol_addrs.insert(symbol.name.clone(), addr);
            }
        }

        let mut patched_chunks = Vec::<PatchedChunk>::new();
        let mut patcher = FilePatcher::new(chunk_starts, symbol_addrs, errors);
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
    /// The address space and start address for each chunk in this object file.
    chunk_starts: &'a [AbsoluteLabel],
    /// For each symbol declared in the object file, local or imported, this
    /// stores the address space and run address of the symbol, or `None` if
    /// the symbol couldn't be resolved.
    symbol_addrs: HashMap<Rc<str>, Option<AbsoluteLabel>>,
    errors: &'a mut Vec<LinkError>,
}

impl<'a> FilePatcher<'a> {
    pub fn new(
        chunk_starts: &'a [AbsoluteLabel],
        symbol_addrs: HashMap<Rc<str>, Option<AbsoluteLabel>>,
        errors: &'a mut Vec<LinkError>,
    ) -> FilePatcher<'a> {
        FilePatcher { chunk_starts, symbol_addrs, errors }
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
                ObjExprOp::BinOp(binop) => {
                    let opt_rhs = value_stack.pop();
                    let opt_lhs = value_stack.pop();
                    match (opt_lhs, opt_rhs) {
                        (Some(lhs), Some(rhs)) => {
                            match binop.evaluate(lhs, rhs) {
                                Ok(result) => value_stack.push(result),
                                Err(_) => {
                                    // TODO: add error details
                                    self.errors.push(
                                        LinkError::PatchEvaluationFailed,
                                    );
                                    return None;
                                }
                            }
                        }
                        _ => {
                            // Stack underflow.  That shouldn't happen if
                            // unless the object file was corrupted.
                            self.errors
                                .push(LinkError::MalformedPatchExpression);
                            return None;
                        }
                    }
                }
                ObjExprOp::LabelAddr => {
                    match value_stack.pop() {
                        Some(ExprValue::Label(label)) => {
                            let resolved = self.resolve_label(label)?;
                            let address = BigInt::from(resolved.address);
                            value_stack.push(ExprValue::Integer(address));
                        }
                        _ => {
                            // The expression has a type error.  That shouldn't
                            // happen if unless the object file was corrupted.
                            self.errors
                                .push(LinkError::MalformedPatchExpression);
                            return None;
                        }
                    }
                }
                ObjExprOp::Push(ExprValue::Label(label)) => {
                    let resolved = self.resolve_label(label)?;
                    value_stack.push(ExprValue::Label(
                        ExprLabel::AddrAbsolute {
                            space: resolved.space,
                            address: BigInt::from(resolved.address),
                        },
                    ));
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

    fn resolve_label(&mut self, label: ExprLabel) -> Option<AbsoluteLabel> {
        match label {
            ExprLabel::AddrAbsolute { space, address } => {
                Some(AbsoluteLabel {
                    space,
                    address: Addr::wrap_bigint(&address),
                })
            }
            ExprLabel::ChunkAbsolute { chunk_index, address } => {
                if chunk_index >= self.chunk_starts.len() {
                    // Reference to a chunk that doesn't exist in this object
                    // file.
                    self.errors.push(LinkError::MalformedPatchExpression);
                    return None;
                }
                let chunk_start = self.chunk_starts[chunk_index].clone();
                Some(AbsoluteLabel {
                    space: chunk_start.space,
                    address: Addr::wrap_bigint(&address),
                })
            }
            ExprLabel::ChunkRelative { chunk_index, offset } => {
                if chunk_index >= self.chunk_starts.len() {
                    // Reference to a chunk that doesn't exist in this object
                    // file.
                    self.errors.push(LinkError::MalformedPatchExpression);
                    return None;
                }
                let chunk_start = self.chunk_starts[chunk_index].clone();
                Some(chunk_start.plus_offset(&offset))
            }
            ExprLabel::SymbolRelative { name, offset } => {
                match self.symbol_addrs.get(&name) {
                    None => {
                        // Reference to a symbol not declared in this object
                        // file.
                        self.errors.push(LinkError::MalformedPatchExpression);
                        None
                    }
                    Some(None) => {
                        // Imported symbol that was never exported; an error
                        // was already reported for this.
                        None
                    }
                    Some(Some(symbol)) => Some(symbol.plus_offset(&offset)),
                }
            }
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
