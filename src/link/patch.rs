use super::error::{LinkError, LinkResult};
use super::eval::{LinkEvalEnv, LinkSymbolContext};
use super::positioned::PositionedBinary;
use super::types::{AbsoluteLabel, ChunkMetadata};
use crate::addr::{Align, Offset};
use crate::error::Errs;
use crate::expr::{ExprType, ExprValue};
use crate::obj::{
    ObjChunk, ObjExpr, ObjFile, ObjPatch, ObjPatchData, ObjPatchIntType,
    ObjPatchRelType, ObjSrcLoc,
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
        positioned_binary: &PositionedBinary,
    ) -> LinkResult<Vec<PatchedFile>> {
        debug_assert_eq!(
            object_files.len(),
            positioned_binary.file_chunk_metadata.len()
        );
        debug_assert_eq!(
            object_files.len(),
            positioned_binary.internal_symbols.len()
        );
        let mut errs = Errs::<LinkError>::new();
        let patched_files = object_files
            .into_iter()
            .zip(&positioned_binary.file_chunk_metadata)
            .zip(&positioned_binary.internal_symbols)
            .filter_map(|((file, chunk_metadata), internal_symbols)| {
                errs.ok(PatchedFile::patch(
                    file,
                    chunk_metadata,
                    &positioned_binary.address_space_widths,
                    &positioned_binary.external_symbols,
                    internal_symbols,
                ))
            })
            .collect::<Vec<PatchedFile>>();
        errs.result()?;
        Ok(patched_files)
    }

    fn patch(
        object_file: ObjFile,
        chunk_metadata: &[ChunkMetadata],
        address_space_widths: &HashMap<Rc<str>, Align>,
        external_symbols: &HashMap<Rc<str>, (AbsoluteLabel, ObjSrcLoc)>,
        internal_symbols: &HashMap<Rc<str>, AbsoluteLabel>,
    ) -> LinkResult<PatchedFile> {
        let mut errs = Errs::<LinkError>::new();
        let mut symbol_addrs =
            HashMap::<Rc<str>, Option<AbsoluteLabel>>::new();
        for import in object_file.imports {
            let opt_absolute_label = external_symbols
                .get(&import.full_name)
                .map(|(label, _)| label.clone());
            if opt_absolute_label.is_none() {
                errs.push(LinkError::SymbolImportUnresolved {
                    symbol_name: import.full_name.clone(),
                    import_loc: import.loc,
                });
            }
            symbol_addrs.insert(import.full_name, opt_absolute_label);
        }
        for chunk in &object_file.chunks {
            for symbol in chunk.symbols.iter() {
                let absolute_label = if symbol.exported {
                    external_symbols.get(&symbol.name).unwrap().0.clone()
                } else {
                    internal_symbols.get(&symbol.name).unwrap().clone()
                };
                symbol_addrs.insert(symbol.name.clone(), Some(absolute_label));
            }
        }

        let mut patcher = FilePatcher::new(
            chunk_metadata,
            address_space_widths,
            symbol_addrs,
        );
        errs.also(patcher.evaluate_variables(object_file.variables));
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
    /// The offset from the start of the chunk, in bytes, for each symbol
    /// exported by the chunk.
    pub exported_symbols: HashMap<Rc<str>, Offset>,
}

impl PatchedChunk {
    pub fn take_data(&mut self) -> Box<[u8]> {
        std::mem::take(&mut self.data)
    }
}

//===========================================================================//

struct FilePatcher<'a> {
    context: LinkSymbolContext<'a>,
    env: LinkEvalEnv,
}

impl<'a> FilePatcher<'a> {
    pub fn new(
        chunk_metadata: &'a [ChunkMetadata],
        address_space_widths: &'a HashMap<Rc<str>, Align>,
        symbol_addrs: HashMap<Rc<str>, Option<AbsoluteLabel>>,
    ) -> FilePatcher<'a> {
        FilePatcher {
            context: LinkSymbolContext {
                address_space_widths,
                chunk_metadata,
                symbol_addrs,
            },
            env: LinkEvalEnv::new(),
        }
    }

    pub fn evaluate_variables(
        &mut self,
        variables: Vec<ObjExpr>,
    ) -> LinkResult<()> {
        let mut errs = Errs::<LinkError>::new();
        for expr in variables {
            errs.also(self.env.evaluate_variable(&expr, &self.context));
        }
        errs.result()
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
        let exported_symbols = chunk
            .symbols
            .into_iter()
            .filter(|symbol| symbol.exported)
            .map(|symbol| (symbol.name, symbol.offset))
            .collect::<HashMap<Rc<str>, Offset>>();
        Ok(PatchedChunk { data, exported_symbols })
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
                    self.apply_fill_patch(chunk_index, size, start, data)
                }
                ObjPatchData::Integer(int_type, expr) => self.apply_int_patch(
                    chunk_index,
                    int_type,
                    expr,
                    start,
                    data,
                ),
                ObjPatchData::Relative(rel_type, lhs, rhs) => {
                    self.apply_rel_patch(rel_type, lhs, rhs, start, data)
                }
            }
        } else {
            Err(Errs::one(LinkError::PatchOffsetOutOfRange))
        }
    }

    fn apply_fill_patch(
        &self,
        chunk_index: usize,
        size: usize,
        start: usize,
        data: &mut [u8],
    ) -> LinkResult<()> {
        let metadata = &self.context.chunk_metadata[chunk_index];
        write_fill_patch(metadata.fill, size, start, data);
        Ok(())
    }

    fn apply_int_patch(
        &self,
        chunk_index: usize,
        int_type: ObjPatchIntType,
        expr: ObjExpr,
        start: usize,
        data: &mut [u8],
    ) -> LinkResult<()> {
        let bigint = if int_type.is_address() {
            let absolute = self.eval_addr_expr(&expr)?;
            let metadata = &self.context.chunk_metadata[chunk_index];
            if absolute.space.name != metadata.start.space.name {
                return Err(Errs::one(LinkError::LabelInWrongAddressSpace {
                    expr_loc: expr.loc,
                    actual_address: absolute.address,
                    address_width: absolute.space.width,
                    actual_space: absolute.space.name,
                    required_space: metadata.start.space.name.clone(),
                }));
            }
            BigInt::from(absolute.address)
        } else {
            self.eval_int_expr(&expr)?
        };
        write_int_patch(int_type, expr.loc, bigint, start, data)
    }

    fn apply_rel_patch(
        &self,
        rel_type: ObjPatchRelType,
        lhs_expr: ObjExpr,
        rhs_expr: ObjExpr,
        start: usize,
        data: &mut [u8],
    ) -> LinkResult<()> {
        let mut errs = Errs::<LinkError>::new();
        let lhs = errs.ok(self.eval_addr_expr(&lhs_expr));
        let rhs = errs.ok(self.eval_addr_expr(&rhs_expr));
        if let (Some(lhs), Some(rhs)) = (lhs, rhs) {
            if lhs.space.name != rhs.space.name {
                errs.push(LinkError::Misc);
            } else {
                errs.also(write_rel_patch(rel_type, lhs, rhs, start, data))
            }
        }
        errs.result()
    }

    fn eval_addr_expr(&self, expr: &ObjExpr) -> LinkResult<AbsoluteLabel> {
        match self.eval_patch_expr(expr)? {
            ExprValue::Label(label) => self.context.resolve_label(&label),
            expr_value => Err(Errs::one(LinkError::PatchValueWrongType {
                expr_loc: expr.loc.clone(),
                expr_value,
                required_type: ExprType::Label,
            })),
        }
    }

    fn eval_int_expr(&self, expr: &ObjExpr) -> LinkResult<BigInt> {
        match self.eval_patch_expr(expr)? {
            ExprValue::Integer(bigint) => Ok(bigint),
            expr_value => Err(Errs::one(LinkError::PatchValueWrongType {
                expr_loc: expr.loc.clone(),
                expr_value,
                required_type: ExprType::Integer,
            })),
        }
    }

    /// Evaluates an expression and returns the result value.
    fn eval_patch_expr(&self, expr: &ObjExpr) -> LinkResult<ExprValue> {
        self.env.evaluate_expression(expr, &self.context)
    }
}

//===========================================================================//

fn write_fill_patch(
    fill_byte: u8,
    size: usize,
    offset: usize,
    data: &mut [u8],
) {
    debug_assert!(offset + size <= data.len());
    data[offset..(offset + size)].fill(fill_byte);
}

fn write_int_patch(
    int_type: ObjPatchIntType,
    expr_loc: ObjSrcLoc,
    expr_value: BigInt,
    offset: usize,
    data: &mut [u8],
) -> LinkResult<()> {
    match int_type.value_in_range(&expr_value) {
        Ok(int) => {
            int_type.write_value_at(int, offset, data);
            Ok(())
        }
        Err(_range) => Err(Errs::one(LinkError::PatchValueOutOfRange {
            int_type,
            expr_loc,
            expr_value,
        })),
    }
}

fn write_rel_patch(
    rel_type: ObjPatchRelType,
    lhs: AbsoluteLabel,
    rhs: AbsoluteLabel,
    offset: usize,
    data: &mut [u8],
) -> LinkResult<()> {
    let delta = BigInt::from(lhs.address) - BigInt::from(rhs.address);
    match rel_type.delta_value_in_range(&delta) {
        Ok(delta) => {
            rel_type.write_delta_at(delta, offset, data);
            Ok(())
        }
        Err(range) => Err(Errs::one(LinkError::RelativeAddressOutOfRange {
            delta,
            range,
        })),
    }
}

//===========================================================================//
