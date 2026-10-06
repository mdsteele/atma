use super::check::bigint_range;
use super::env::AsmTypeEnv;
use super::error::{AsmError, AsmResult};
use crate::addr::Endianness;
use crate::error::{Errs, SrcSpan};
use crate::expr::ExprType;
use crate::obj::{ObjPatchData, ObjPatchIntType};
use crate::parse::{AsmIntDataAst, AsmIntType, ExprAst};
use num_bigint::BigInt;

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
            match static_value.unwrap_label().into_absolute_address() {
                Some(address) => errs.with(IntDataValue::integer_static(
                    env, directive, int_type, expr_span, address,
                )),
                None => {
                    IntDataValue::Patch(ObjPatchData::Integer(int_type, expr))
                }
            }
        }
        (_, ExprType::Integer, Ok(static_value)) => {
            let bigint = static_value.unwrap_int();
            errs.with(IntDataValue::integer_static(
                env, directive, int_type, expr_span, bigint,
            ))
        }
        (expr, ExprType::Label, Err(reason))
        | (expr, ExprType::Integer, Err(reason))
        | (expr, ExprType::Bottom, Err(reason)) => {
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
    match int_data_value {
        IntDataValue::Static(static_value) => {
            errs.also(env.with_chunk_data(|chunk_data| {
                int_type.append_value(static_value, chunk_data);
                Ok(())
            }));
        }
        IntDataValue::Patch(patch_data) => {
            errs.also(env.append_chunk_patch(patch_data));
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
                    valid_range: bigint_range(range.start, range.last),
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
        // Address:
        AsmIntType::A8 => Ok(ObjPatchIntType::A8),
        AsmIntType::A16 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::A16be,
            ObjPatchIntType::A16le,
        ),
        AsmIntType::A16be => Ok(ObjPatchIntType::A16be),
        AsmIntType::A16le => Ok(ObjPatchIntType::A16le),
        AsmIntType::A24 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::A24be,
            ObjPatchIntType::A24le,
        ),
        AsmIntType::A24be => Ok(ObjPatchIntType::A24be),
        AsmIntType::A24le => Ok(ObjPatchIntType::A24le),
        AsmIntType::A32 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::A32be,
            ObjPatchIntType::A32le,
        ),
        AsmIntType::A32be => Ok(ObjPatchIntType::A32be),
        AsmIntType::A32le => Ok(ObjPatchIntType::A32le),
        // Signed:
        AsmIntType::S8 => Ok(ObjPatchIntType::S8),
        AsmIntType::S16 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::S16be,
            ObjPatchIntType::S16le,
        ),
        AsmIntType::S16be => Ok(ObjPatchIntType::S16be),
        AsmIntType::S16le => Ok(ObjPatchIntType::S16le),
        AsmIntType::S24 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::S24be,
            ObjPatchIntType::S24le,
        ),
        AsmIntType::S24be => Ok(ObjPatchIntType::S24be),
        AsmIntType::S24le => Ok(ObjPatchIntType::S24le),
        AsmIntType::S32 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::S32be,
            ObjPatchIntType::S32le,
        ),
        AsmIntType::S32be => Ok(ObjPatchIntType::S32be),
        AsmIntType::S32le => Ok(ObjPatchIntType::S32le),
        // Unsigned:
        AsmIntType::U8 => Ok(ObjPatchIntType::U8),
        AsmIntType::U16 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::U16be,
            ObjPatchIntType::U16le,
        ),
        AsmIntType::U16be => Ok(ObjPatchIntType::U16be),
        AsmIntType::U16le => Ok(ObjPatchIntType::U16le),
        AsmIntType::U24 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::U24be,
            ObjPatchIntType::U24le,
        ),
        AsmIntType::U24be => Ok(ObjPatchIntType::U24be),
        AsmIntType::U24le => Ok(ObjPatchIntType::U24le),
        AsmIntType::U32 => endian_patch_type(
            env,
            int_data_ast,
            ObjPatchIntType::U32be,
            ObjPatchIntType::U32le,
        ),
        AsmIntType::U32be => Ok(ObjPatchIntType::U32be),
        AsmIntType::U32le => Ok(ObjPatchIntType::U32le),
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
