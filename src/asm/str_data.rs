use super::env::AsmTypeEnv;
use super::error::{AsmError, AsmResult};
use crate::error::{Errs, SrcSpan};
use crate::expr::ExprType;
use crate::parse::{AsmStrTypeAst, ExprAst};
use num_bigint::BigInt;
use num_traits::ToPrimitive;

//===========================================================================//

pub(super) fn assemble_str_data(
    env: &mut AsmTypeEnv,
    str_type: AsmStrTypeAst,
    expr_ast: ExprAst,
) -> AsmResult<()> {
    let mut errs = Errs::<AsmError>::new();
    let expr_span = expr_ast.span;
    match errs.with(env.typecheck_expression(expr_ast)) {
        (_, ExprType::Undefined, _) => {}
        (_, ExprType::Integer, Ok(value)) => {
            errs.also(assemble_str_data_bigint(
                env,
                str_type,
                expr_span,
                value.unwrap_int_ref(),
            ));
        }
        (_, ExprType::String, Ok(value)) => {
            errs.also(assemble_str_data_string(
                env,
                str_type,
                expr_span,
                value.unwrap_str_ref(),
            ));
        }
        (
            _,
            ExprType::Bottom | ExprType::Integer | ExprType::String,
            Err(reason),
        ) => {
            errs.push(AsmError::DirectiveExprNotStatic {
                directive: str_type.directive(),
                component: "value",
                expr_loc: env.make_loc(expr_span),
                reason,
            });
        }
        (_, expr_type, _) => {
            errs.push(AsmError::DirectiveExprTypeError {
                directive: str_type.directive(),
                component: "value",
                expr_loc: env.make_loc(expr_span),
                expr_type,
                valid_types: vec![ExprType::String, ExprType::Integer],
            });
        }
    }
    errs.result()
}

fn assemble_str_data_bigint(
    env: &mut AsmTypeEnv,
    str_type: AsmStrTypeAst,
    expr_span: SrcSpan,
    bigint: &BigInt,
) -> AsmResult<()> {
    match str_type {
        AsmStrTypeAst::Ascii => {
            if let Some(byte) = bigint.to_u8()
                && byte < 0x80
            {
                env.append_chunk_data(&[byte])
            } else {
                Err(Errs::one(AsmError::InvalidAsciiValue {
                    expr_loc: env.make_loc(expr_span),
                    expr_value: bigint.clone(),
                }))
            }
        }
        AsmStrTypeAst::Utf8 => {
            if let Some(chr) = bigint.to_u32().and_then(char::from_u32) {
                env.append_chunk_data(chr.to_string().as_bytes())
            } else {
                Err(Errs::one(AsmError::InvalidUnicodeScalarValue {
                    expr_loc: env.make_loc(expr_span),
                    expr_value: bigint.clone(),
                }))
            }
        }
    }
}

fn assemble_str_data_string(
    env: &mut AsmTypeEnv,
    str_type: AsmStrTypeAst,
    expr_span: SrcSpan,
    string: &str,
) -> AsmResult<()> {
    match str_type {
        AsmStrTypeAst::Ascii => {
            if let Some(chr) = string.chars().find(|chr| !chr.is_ascii()) {
                Err(Errs::one(AsmError::InvalidAsciiString {
                    expr_loc: env.make_loc(expr_span),
                    non_ascii_char: chr,
                }))
            } else {
                env.append_chunk_data(string.as_bytes())
            }
        }
        AsmStrTypeAst::Utf8 => env.append_chunk_data(string.as_bytes()),
    }
}

//===========================================================================//
