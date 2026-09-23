use super::env::AsmTypeEnv;
use super::error::{AsmError, AsmResult};
use crate::addr::Endianness;
use crate::error::{Errs, SrcSpan};
use crate::expr::{ExprLabel, ExprType, ExprUnOp};
use crate::obj::{ObjExpr, ObjExprOp, ObjPatchData, ObjPatchIntType};
use crate::parse::{AsmIntDataAst, AsmIntTypeAst, ExprAst};
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
    int_data_ast: &AsmIntDataAst,
) -> AsmResult<ObjPatchIntType> {
    match int_data_ast.int_type {
        AsmIntTypeAst::S8 => Ok(ObjPatchIntType::S8),
        AsmIntTypeAst::S16 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::S16be,
            ObjPatchIntType::S16le,
        ),
        AsmIntTypeAst::S16be => Ok(ObjPatchIntType::S16be),
        AsmIntTypeAst::S16le => Ok(ObjPatchIntType::S16le),
        AsmIntTypeAst::S24 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::S24be,
            ObjPatchIntType::S24le,
        ),
        AsmIntTypeAst::S24be => Ok(ObjPatchIntType::S24be),
        AsmIntTypeAst::S24le => Ok(ObjPatchIntType::S24le),
        AsmIntTypeAst::U8 => Ok(ObjPatchIntType::U8),
        AsmIntTypeAst::U16 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::U16be,
            ObjPatchIntType::U16le,
        ),
        AsmIntTypeAst::U16be => Ok(ObjPatchIntType::U16be),
        AsmIntTypeAst::U16le => Ok(ObjPatchIntType::U16le),
        AsmIntTypeAst::U24 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::U24be,
            ObjPatchIntType::U24le,
        ),
        AsmIntTypeAst::U24be => Ok(ObjPatchIntType::U24be),
        AsmIntTypeAst::U24le => Ok(ObjPatchIntType::U24le),
    }
}

fn endian_patch_type(
    env: &AsmTypeEnv,
    int_data_ast: &AsmIntDataAst,
    be_type: ObjPatchIntType,
    le_type: ObjPatchIntType,
) -> AsmResult<ObjPatchIntType> {
    let arch = env.current_arch();
    match env.arch_tree().native_endianness(arch) {
        Some(Endianness::BigEndian) => Ok(be_type),
        Some(Endianness::LittleEndian) => Ok(le_type),
        None => Err(Errs::one(AsmError::ArchHasNoEndianness {
            directive: int_data_ast.int_type.directive(),
            loc: env.make_loc(int_data_ast.directive_span),
            arch: env.current_arch().clone(),
        })),
    }
}

//===========================================================================//
