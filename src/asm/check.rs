//! Typechecking utilities common to multiple assembly directives.

use super::error::{AsmError, AsmResult};
use crate::asm::env::AsmTypeEnv;
use crate::error::Errs;
use crate::expr::{ExprStatic, ExprType, ExprValue};
use crate::obj::ObjExpr;
use crate::parse::ExprAst;
use num_bigint::BigInt;
use std::range::RangeInclusive;

//===========================================================================//

pub(super) fn typecheck_static_dir_expr_as(
    env: &AsmTypeEnv,
    (directive, component): (&'static str, &'static str),
    expr_ast: ExprAst,
    required_type: ExprType,
) -> AsmResult<ExprValue> {
    let expr_span = expr_ast.span;
    let (_, expr_static) = typecheck_dir_expr_as(
        env,
        (directive, component),
        expr_ast,
        required_type,
    )?;
    match expr_static {
        Ok(value) => Ok(value),
        Err(reason) => Err(Errs::one(AsmError::DirectiveExprNotStatic {
            directive,
            component,
            expr_loc: env.make_loc(expr_span),
            reason,
        })),
    }
}

pub(super) fn typecheck_dir_expr_as(
    env: &AsmTypeEnv,
    (directive, component): (&'static str, &'static str),
    expr_ast: ExprAst,
    required_type: ExprType,
) -> AsmResult<(ObjExpr, ExprStatic)> {
    let mut errs = Errs::<AsmError>::new();
    let expr_span = expr_ast.span;
    let (expr, expr_type, expr_static) =
        errs.with(env.typecheck_expression(expr_ast));
    if !expr_type.is_subtype_of(&required_type) {
        errs.push(AsmError::DirectiveExprTypeError {
            directive,
            component,
            expr_loc: env.make_loc(expr_span),
            expr_type,
            valid_types: vec![required_type],
        });
    }
    errs.result()?;
    Ok((expr, expr_static))
}

//===========================================================================//

pub(super) fn bigint_range<T: Into<BigInt>>(
    start: T,
    last: T,
) -> RangeInclusive<BigInt> {
    RangeInclusive { start: start.into(), last: last.into() }
}

//===========================================================================//
