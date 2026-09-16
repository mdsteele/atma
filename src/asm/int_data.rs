use super::env::AsmTypeEnv;
use super::error::{AsmError, AsmResult};
use crate::addr::Endianness;
use crate::error::{Errs, SrcSpan};
use crate::expr::{ExprLabel, ExprType, ExprUnOp};
use crate::obj::{ObjExpr, ObjExprOp, ObjPatchData, ObjPatchIntType};
use crate::parse::{AsmIntTypeAst, ExprAst};
use num_bigint::BigInt;
use std::range::RangeInclusive;

//===========================================================================//

pub(super) fn assemble_int_data(
    env: &mut AsmTypeEnv,
    directive: &'static str,
    int_type: ObjPatchIntType,
    expr_ast: ExprAst,
) -> AsmResult<()> {
    let mut errs = Errs::<AsmError>::new();
    let expr_span = expr_ast.span;
    let int_data_value = match errs.with(env.typecheck_expression(expr_ast)) {
        (_, ExprType::Undefined, _) => IntDataValue::default(),
        (expr, ExprType::Label, Ok(static_value)) => {
            match static_value.unwrap_label() {
                ExprLabel::AddrAbsolute { address, .. }
                | ExprLabel::ChunkAbsolute { address, .. } => {
                    errs.with(IntDataValue::integer_static(
                        env, directive, int_type, expr_span, address,
                    ))
                }
                ExprLabel::ChunkRelative { .. }
                | ExprLabel::SymbolRelative { .. } => {
                    IntDataValue::label_patch(env, int_type, expr_span, expr)
                }
            }
        }
        (expr, ExprType::Label, Err(reason)) => {
            errs.also(env.check_for_inevitable_eval_error(&reason));
            IntDataValue::label_patch(env, int_type, expr_span, expr)
        }
        (_, ExprType::Integer, Ok(static_value)) => {
            let bigint = static_value.unwrap_int();
            errs.with(IntDataValue::integer_static(
                env, directive, int_type, expr_span, bigint,
            ))
        }
        (expr, ExprType::Integer | ExprType::Bottom, Err(reason)) => {
            errs.also(env.check_for_inevitable_eval_error(&reason));
            IntDataValue::Patch(ObjPatchData::Integer(int_type, expr))
        }
        (_, expr_type, _) => {
            errs.push(AsmError::DirectiveExprTypeError {
                directive,
                component: "value",
                expr_loc: env.make_loc(expr_span),
                expr_type,
                valid_types: vec![ExprType::Integer, ExprType::Label],
            });
            IntDataValue::default()
        }
    };
    if let Some(chunk) = env.current_chunk_mut() {
        match int_data_value {
            IntDataValue::Static(static_value) => {
                int_type.append_value(static_value, chunk.data_mut());
            }
            IntDataValue::Patch(patch_data) => {
                errs.also(chunk.append_patch(patch_data));
            }
        }
    }
    errs.result()
}

//===========================================================================//

enum IntDataValue {
    Static(i64),
    Patch(ObjPatchData),
}

impl IntDataValue {
    pub fn label_patch(
        env: &AsmTypeEnv,
        int_type: ObjPatchIntType,
        expr_span: SrcSpan,
        mut expr: ObjExpr,
    ) -> Self {
        expr.ops.push(ObjExprOp::UnOp {
            context: env.current_src_context(),
            unop: ExprUnOp::AddrOf,
            op_span: expr_span,
            arg_span: expr_span,
        });
        Self::Patch(ObjPatchData::Integer(int_type, expr))
    }

    pub fn integer_static(
        env: &AsmTypeEnv,
        directive: &'static str,
        int_type: ObjPatchIntType,
        expr_span: SrcSpan,
        bigint: BigInt,
    ) -> (Self, Errs<AsmError>) {
        let mut errs = Errs::<AsmError>::new();
        let int_data_value = match int_type.value_in_range(&bigint) {
            Ok(value) => Self::Static(value),
            Err(range) => {
                errs.push(AsmError::DirectiveExprOutOfRange {
                    directive,
                    component: "value",
                    expr_loc: env.make_loc(expr_span),
                    expr_value: bigint,
                    valid_range: RangeInclusive {
                        start: BigInt::from(range.start),
                        last: BigInt::from(range.last),
                    },
                });
                Self::default()
            }
        };
        (int_data_value, errs)
    }
}

impl Default for IntDataValue {
    fn default() -> Self {
        Self::Static(0)
    }
}

//===========================================================================//

pub(super) fn int_patch_type(
    env: &AsmTypeEnv,
    int_type: AsmIntTypeAst,
) -> Option<ObjPatchIntType> {
    match int_type {
        AsmIntTypeAst::S8 => Some(ObjPatchIntType::S8),
        AsmIntTypeAst::S16 => endian_patch_type(
            env,
            ObjPatchIntType::S16be,
            ObjPatchIntType::S16le,
        ),
        AsmIntTypeAst::S16be => Some(ObjPatchIntType::S16be),
        AsmIntTypeAst::S16le => Some(ObjPatchIntType::S16le),
        AsmIntTypeAst::S24 => endian_patch_type(
            env,
            ObjPatchIntType::S24be,
            ObjPatchIntType::S24le,
        ),
        AsmIntTypeAst::S24be => Some(ObjPatchIntType::S24be),
        AsmIntTypeAst::S24le => Some(ObjPatchIntType::S24le),
        AsmIntTypeAst::U8 => Some(ObjPatchIntType::U8),
        AsmIntTypeAst::U16 => endian_patch_type(
            env,
            ObjPatchIntType::U16be,
            ObjPatchIntType::U16le,
        ),
        AsmIntTypeAst::U16be => Some(ObjPatchIntType::U16be),
        AsmIntTypeAst::U16le => Some(ObjPatchIntType::U16le),
        AsmIntTypeAst::U24 => endian_patch_type(
            env,
            ObjPatchIntType::U24be,
            ObjPatchIntType::U24le,
        ),
        AsmIntTypeAst::U24be => Some(ObjPatchIntType::U24be),
        AsmIntTypeAst::U24le => Some(ObjPatchIntType::U24le),
    }
}

fn endian_patch_type(
    env: &AsmTypeEnv,
    be_type: ObjPatchIntType,
    le_type: ObjPatchIntType,
) -> Option<ObjPatchIntType> {
    let arch = env.current_arch();
    match env.arch_tree().native_endianness(arch) {
        Some(Endianness::BigEndian) => Some(be_type),
        Some(Endianness::LittleEndian) => Some(le_type),
        None => None,
    }
}

//===========================================================================//
