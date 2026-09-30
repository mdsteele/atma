//! Typechecking utilities for chunk directives.

use super::check::{bigint_range, typecheck_static_dir_expr_as};
use super::env::AsmTypeEnv;
use super::error::{AsmError, AsmResult};
use crate::addr::{Addr, Align};
use crate::error::{Errs, SrcSpan};
use crate::expr::ExprType;
use crate::parse::{AsmChunkKind, ExprAst, IdentifierAst};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

#[derive(Default)]
pub(super) struct AsmChunkAttrs {
    pub align: Option<Align>,
    pub arch: Option<Rc<str>>,
    pub fill: Option<u8>,
    pub start: Option<Addr>,
    pub within: Option<Align>,
}

pub(super) fn typecheck_chunk_attrs(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    attrs_ast: Vec<(IdentifierAst, ExprAst)>,
) -> (AsmChunkAttrs, Errs<AsmError>) {
    let mut errs = Errs::<AsmError>::new();
    let mut attrs = AsmChunkAttrs::default();
    let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
    for (id_ast, expr_ast) in attrs_ast {
        errs.also(chunk_declare_attr(env, kind, &mut prev_attrs, &id_ast));
        match &*id_ast.name {
            "align" => {
                attrs.align = Some(
                    errs.ok_or_default(chunk_align_attr(env, kind, expr_ast)),
                )
            }
            "arch" => {
                attrs.arch =
                    Some(errs.ok_or_else(
                        chunk_arch_attr(env, kind, expr_ast),
                        || env.current_arch().clone(),
                    ))
            }
            "fill" => {
                attrs.fill = Some(
                    errs.ok_or_default(chunk_fill_attr(env, kind, expr_ast)),
                )
            }
            "start" => {
                attrs.start = Some(
                    errs.ok_or_default(chunk_start_attr(env, kind, expr_ast)),
                )
            }
            "within" => {
                attrs.within =
                    Some(errs.ok_or(
                        chunk_within_attr(env, kind, expr_ast),
                        Align::MAX,
                    ))
            }
            _ => {
                errs.push(AsmError::InvalidAttrName {
                    directive: kind.directive(),
                    attr_name: id_ast.name,
                    attr_loc: env.make_loc(id_ast.span),
                });
            }
        }
    }
    (attrs, errs)
}

fn chunk_declare_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    prev_attrs: &mut HashMap<Rc<str>, SrcSpan>,
    id_ast: &IdentifierAst,
) -> AsmResult<()> {
    if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
        Err(Errs::one(AsmError::DuplicateAttrName {
            directive: kind.directive(),
            attr_name: id_ast.name.clone(),
            attr_loc: env.make_loc(id_ast.span),
            prev_loc: env.make_loc(prev_span),
        }))
    } else {
        prev_attrs.insert(id_ast.name.clone(), id_ast.span);
        Ok(())
    }
}

fn chunk_align_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    expr_ast: ExprAst,
) -> AsmResult<Align> {
    chunk_static_align_attr(env, kind, "align", expr_ast)
}

fn chunk_arch_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    expr_ast: ExprAst,
) -> AsmResult<Rc<str>> {
    let expr_span = expr_ast.span;
    let arch = chunk_static_str_attr(env, kind, "arch", expr_ast)?;
    if env.arch_tree().contains_arch(&arch) {
        Ok(arch)
    } else {
        Err(Errs::one(AsmError::UnknownArch {
            arch: arch.clone(),
            loc: env.make_loc(expr_span),
        }))
    }
}

fn chunk_fill_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    expr_ast: ExprAst,
) -> AsmResult<u8> {
    let expr_span = expr_ast.span;
    let bigint = chunk_static_int_attr(env, kind, "fill", expr_ast)?;
    u8::try_from(&bigint).map_err(|_| {
        Errs::one(AsmError::DirectiveExprOutOfRange {
            directive: kind.directive(),
            component: "fill",
            expr_loc: env.make_loc(expr_span),
            expr_value: bigint,
            valid_range: bigint_range(u8::MIN, u8::MAX),
        })
    })
}

fn chunk_start_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    expr_ast: ExprAst,
) -> AsmResult<Addr> {
    let expr_span = expr_ast.span;
    let bigint = chunk_static_int_attr(env, kind, "start", expr_ast)?;
    Addr::try_from(&bigint).map_err(|_| {
        Errs::one(AsmError::DirectiveExprOutOfRange {
            directive: kind.directive(),
            component: "start",
            expr_loc: env.make_loc(expr_span),
            expr_value: bigint,
            valid_range: bigint_range(Addr::MIN, Addr::MAX),
        })
    })
}

fn chunk_within_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    expr_ast: ExprAst,
) -> AsmResult<Align> {
    chunk_static_align_attr(env, kind, "within", expr_ast)
}

fn chunk_static_align_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    attr_name: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<Align> {
    let expr_span = expr_ast.span;
    let bigint = chunk_static_int_attr(env, kind, attr_name, expr_ast)?;
    Align::try_from(&bigint).map_err(|error| {
        Errs::one(AsmError::InvalidAlignmentValue {
            directive: kind.directive(),
            attr_name,
            error,
            expr_loc: env.make_loc(expr_span),
            expr_value: bigint,
        })
    })
}

fn chunk_static_int_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    attr_name: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<BigInt> {
    typecheck_static_dir_expr_as(
        env,
        (kind.directive(), attr_name),
        expr_ast,
        ExprType::Integer,
    )
    .map(|value| value.unwrap_int())
}

fn chunk_static_str_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    attr_name: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<Rc<str>> {
    typecheck_static_dir_expr_as(
        env,
        (kind.directive(), attr_name),
        expr_ast,
        ExprType::String,
    )
    .map(|value| value.unwrap_str())
}

//===========================================================================//

pub(super) fn validate_chunk_location(
    env: &AsmTypeEnv,
    directive_span: SrcSpan,
    kind: AsmChunkKind,
) -> AsmResult<()> {
    match kind {
        AsmChunkKind::Section => {
            if !env.is_at_top_level() {
                return Err(Errs::one(AsmError::DirectiveNotAtTopLevel {
                    directive: kind.directive(),
                    loc: env.make_loc(directive_span),
                }));
            }
        }
        AsmChunkKind::Elsewhere | AsmChunkKind::Loadable => {
            if env.current_chunk().is_none() {
                return Err(Errs::one(AsmError::DirectiveNotInSection {
                    directive: kind.directive(),
                    loc: env.make_loc(directive_span),
                }));
            }
        }
    }
    Ok(())
}

//===========================================================================//
