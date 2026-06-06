use super::error::{LinkError, LinkResult};
use super::types::{AbsoluteLabel, ChunkMetadata};
use crate::addr::Addr;
use crate::error::Errs;
use crate::expr::{ExprLabel, ExprValue};
use crate::obj::{
    ObjAssert, ObjChunk, ObjExpr, ObjExprOp, ObjFile, ObjPatch, ObjPatchData,
    ObjPatchIntType,
};
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
        file_chunk_metadata: &[Vec<ChunkMetadata>],
        exported_symbols: &HashMap<Rc<str>, AbsoluteLabel>,
    ) -> LinkResult<Vec<PatchedFile>> {
        debug_assert_eq!(object_files.len(), file_chunk_metadata.len());
        let mut errs = Errs::<LinkError>::new();
        let patched_files = object_files
            .into_iter()
            .zip(file_chunk_metadata)
            .filter_map(|(file, chunk_metadata)| {
                errs.ok(PatchedFile::patch(
                    file,
                    chunk_metadata,
                    exported_symbols,
                ))
            })
            .collect::<Vec<PatchedFile>>();
        errs.result()?;
        Ok(patched_files)
    }

    fn patch(
        object_file: ObjFile,
        chunk_metadata: &[ChunkMetadata],
        exported_symbols: &HashMap<Rc<str>, AbsoluteLabel>,
    ) -> LinkResult<PatchedFile> {
        let mut errs = Errs::<LinkError>::new();
        let mut symbol_addrs =
            HashMap::<Rc<str>, Option<AbsoluteLabel>>::new();
        for name in object_file.imports {
            let addr = exported_symbols.get(&name).cloned();
            if addr.is_none() {
                errs.push(LinkError::SymbolImportUnresolved {
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
                    errs.push(LinkError::SymbolImportUnresolved {
                        symbol_name: symbol.name.clone(),
                    });
                }
                symbol_addrs.insert(symbol.name.clone(), addr);
            }
        }

        let patcher = FilePatcher::new(chunk_metadata, symbol_addrs);
        if errs.ok(patcher.check_assertions(object_file.asserts)).is_none() {
            assert!(!errs.is_empty());
            return Err(errs);
        }
        let mut patched_chunks = Vec::<PatchedChunk>::new();
        for (chunk_index, chunk) in object_file.chunks.into_iter().enumerate()
        {
            if let Some(patched_chunk) =
                errs.ok(patcher.patch_chunk(chunk_index, chunk))
            {
                patched_chunks.push(patched_chunk);
            }
        }

        errs.result()?;
        Ok(PatchedFile { chunks: patched_chunks })
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
    /// Metadata for each chunk in this object file.
    chunk_metadata: &'a [ChunkMetadata],
    /// For each symbol declared in the object file, local or imported, this
    /// stores the address space and run address of the symbol, or `None` if
    /// the symbol couldn't be resolved.
    symbol_addrs: HashMap<Rc<str>, Option<AbsoluteLabel>>,
}

impl<'a> FilePatcher<'a> {
    pub fn new(
        chunk_metadata: &'a [ChunkMetadata],
        symbol_addrs: HashMap<Rc<str>, Option<AbsoluteLabel>>,
    ) -> FilePatcher<'a> {
        FilePatcher { chunk_metadata, symbol_addrs }
    }

    pub fn check_assertions(&self, asserts: Vec<ObjAssert>) -> LinkResult<()> {
        let mut errs = Errs::<LinkError>::new();
        for assert in asserts {
            errs.also(self.check_assertion(assert));
        }
        errs.result()
    }

    fn check_assertion(&self, assert: ObjAssert) -> LinkResult<()> {
        match self.eval_patch_expr(assert.condition)? {
            ExprValue::Boolean(true) => Ok(()),
            ExprValue::Boolean(false) => {
                if let Some(message_expr) = assert.message {
                    // TODO: if assertion fails AND evaluation of message
                    // string has errors, report the failed assertion in
                    // addition to the message string errors.
                    match self.eval_patch_expr(message_expr)? {
                        ExprValue::String(message_string) => {
                            Err(Errs::one(LinkError::AssertionFailed {
                                message: Some(message_string),
                            }))
                        }
                        _ => Err(Errs::one(LinkError::PatchValueWrongType)),
                    }
                } else {
                    Err(Errs::one(LinkError::AssertionFailed {
                        message: None,
                    }))
                }
            }
            _ => Err(Errs::one(LinkError::PatchValueWrongType)),
        }
    }

    pub fn patch_chunk(
        &self,
        chunk_index: usize,
        chunk: ObjChunk,
    ) -> LinkResult<PatchedChunk> {
        let mut errs = Errs::<LinkError>::new();
        let mut data = chunk.data;
        for patch in chunk.patches.into_iter() {
            errs.also(self.apply_patch(chunk_index, patch, &mut data));
        }
        errs.result()?;
        Ok(PatchedChunk { data })
    }

    fn apply_patch(
        &self,
        chunk_index: usize,
        patch: ObjPatch,
        data: &mut [u8],
    ) -> LinkResult<()> {
        if let Ok(start) = usize::try_from(patch.offset)
            && let Some(end) = start.checked_add(patch.data.num_bytes())
            && end <= data.len()
        {
            match patch.data {
                ObjPatchData::Fill(size) => {
                    let metadata = &self.chunk_metadata[chunk_index];
                    write_fill_patch(start, size, metadata.fill, data);
                    Ok(())
                }
                ObjPatchData::Integer(int_type, expr) => {
                    match self.eval_patch_expr(expr)? {
                        ExprValue::Integer(bigint) => {
                            match int_type.value_in_range(&bigint) {
                                Ok(int) => {
                                    write_int_patch(
                                        int_type, start, int, data,
                                    );
                                    Ok(())
                                }
                                Err(_range) => Err(Errs::one(
                                    LinkError::PatchValueOutOfRange {
                                        int_type,
                                        value: bigint,
                                    },
                                )),
                            }
                        }
                        _ => Err(Errs::one(LinkError::PatchValueWrongType)),
                    }
                }
            }
        } else {
            Err(Errs::one(LinkError::PatchOffsetOutOfRange))
        }
    }

    /// Evaluates an expression and returns the result value.
    fn eval_patch_expr(&self, expr: ObjExpr) -> LinkResult<ExprValue> {
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
                                    return Err(Errs::one(
                                        LinkError::PatchEvaluationFailed,
                                    ));
                                }
                            }
                        }
                        _ => {
                            // Stack underflow.  That shouldn't happen if
                            // unless the object file was corrupted.
                            return Err(Errs::one(
                                LinkError::MalformedPatchExpression,
                            ));
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
                            return Err(Errs::one(
                                LinkError::MalformedPatchExpression,
                            ));
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
            Ok(value)
        } else {
            Err(Errs::one(LinkError::MalformedPatchExpression))
        }
    }

    fn resolve_label(&self, label: ExprLabel) -> LinkResult<AbsoluteLabel> {
        match label {
            ExprLabel::AddrAbsolute { space, address } => Ok(AbsoluteLabel {
                space,
                address: Addr::wrap_bigint(&address),
            }),
            ExprLabel::ChunkAbsolute { chunk_index, address } => {
                if chunk_index >= self.chunk_metadata.len() {
                    // Reference to a chunk that doesn't exist in this object
                    // file.
                    return Err(Errs::one(
                        LinkError::MalformedPatchExpression,
                    ));
                }
                let metadata = &self.chunk_metadata[chunk_index];
                let space = metadata.start.space.clone();
                Ok(AbsoluteLabel {
                    space,
                    address: Addr::wrap_bigint(&address),
                })
            }
            ExprLabel::ChunkRelative { chunk_index, offset } => {
                if chunk_index >= self.chunk_metadata.len() {
                    // Reference to a chunk that doesn't exist in this object
                    // file.
                    return Err(Errs::one(
                        LinkError::MalformedPatchExpression,
                    ));
                }
                let metadata = &self.chunk_metadata[chunk_index];
                let chunk_start = metadata.start.clone();
                Ok(chunk_start.plus_offset(&offset))
            }
            ExprLabel::SymbolRelative { name, offset } => {
                match self.symbol_addrs.get(&name) {
                    None => {
                        // Reference to a symbol not declared in this object
                        // file.
                        Err(Errs::one(LinkError::MalformedPatchExpression))
                    }
                    Some(None) => {
                        // Imported symbol that was never exported; an error
                        // was already reported for this.
                        //
                        // TODO: Is there a better way to handle this than
                        // returning an empty error list?
                        Err(Errs::new())
                    }
                    Some(Some(symbol)) => Ok(symbol.plus_offset(&offset)),
                }
            }
        }
    }
}

//===========================================================================//

fn write_fill_patch(
    offset: usize,
    size: usize,
    fill_byte: u8,
    data: &mut [u8],
) {
    debug_assert!(offset + size <= data.len());
    data[offset..(offset + size)].fill(fill_byte);
}

fn write_int_patch(
    int_type: ObjPatchIntType,
    offset: usize,
    value: i64,
    data: &mut [u8],
) {
    debug_assert!(offset + int_type.num_bytes() <= data.len());
    match int_type {
        ObjPatchIntType::U8 => {
            data[offset] = value as u8;
        }
        ObjPatchIntType::U16be => {
            data[offset] = (value >> 8) as u8;
            data[offset + 1] = value as u8;
        }
        ObjPatchIntType::U16le => {
            data[offset] = value as u8;
            data[offset + 1] = (value >> 8) as u8;
        }
        ObjPatchIntType::U24be => {
            data[offset] = (value >> 16) as u8;
            data[offset + 1] = (value >> 8) as u8;
            data[offset + 2] = value as u8;
        }
        ObjPatchIntType::U24le => {
            data[offset] = value as u8;
            data[offset + 1] = (value >> 8) as u8;
            data[offset + 2] = (value >> 16) as u8;
        }
    }
}

//===========================================================================//
