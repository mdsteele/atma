//! Typechecking utilities for chunk directives.

use super::check::{bigint_range, typecheck_static_dir_expr_as};
use super::env::AsmTypeEnv;
use super::error::{AsmError, AsmResult};
use crate::addr::{Addr, Align};
use crate::error::{Errs, SrcSpan};
use crate::expr::ExprType;
use crate::parse::{AsmChunkKind, AsmWithAst, ExprAst, IdentifierAst};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

#[derive(Default)]
pub(super) struct AsmChunkAttrs {
    pub align: Option<Align>,
    pub arch: Option<Rc<str>>,
    pub charmap: Option<Rc<str>>,
    pub fill: Option<u8>,
    pub start: Option<Addr>,
    pub within: Option<Align>,
}

impl AsmChunkAttrs {
    pub(super) fn build(
        env: &AsmTypeEnv,
        kind: AsmChunkKind,
        attrs_ast: Vec<(IdentifierAst, ExprAst)>,
    ) -> (AsmChunkAttrs, Errs<AsmError>) {
        let mut errs = Errs::<AsmError>::new();
        let directive = kind.directive();
        let mut attrs = AsmChunkAttrs::default();
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for (id_ast, expr_ast) in attrs_ast {
            errs.also(declare_attr(env, directive, &mut prev_attrs, &id_ast));
            match &*id_ast.name {
                "align" => {
                    attrs.align = Some(
                        errs.ok_or_default(align_attr(env, kind, expr_ast)),
                    )
                }
                "arch" => {
                    attrs.arch = errs.ok(arch_attr(env, directive, expr_ast));
                }
                "charmap" => {
                    attrs.charmap =
                        errs.ok(charmap_attr(env, directive, expr_ast));
                }
                "fill" => {
                    attrs.fill =
                        Some(errs.ok_or_default(fill_attr(
                            env, directive, expr_ast,
                        )))
                }
                "start" => {
                    attrs.start = Some(
                        errs.ok_or_default(start_attr(env, kind, expr_ast)),
                    )
                }
                "within" => {
                    attrs.within =
                        Some(errs.ok_or(
                            within_attr(env, kind, expr_ast),
                            Align::MAX,
                        ))
                }
                _ => {
                    errs.push(AsmError::InvalidAttrName {
                        directive,
                        attr_name: id_ast.name,
                        attr_loc: env.make_loc(id_ast.span),
                    });
                }
            }
        }
        (attrs, errs)
    }
}

//===========================================================================//

#[derive(Default)]
pub(super) struct AsmWithAttrs {
    pub arch: Option<Rc<str>>,
    pub charmap: Option<Rc<str>>,
    pub fill: Option<u8>,
}

impl AsmWithAttrs {
    pub(super) fn build(
        env: &AsmTypeEnv,
        attrs_ast: Vec<(IdentifierAst, ExprAst)>,
    ) -> (Self, Errs<AsmError>) {
        let mut errs = Errs::<AsmError>::new();
        let directive = AsmWithAst::DIRECTIVE;
        let mut attrs = AsmWithAttrs::default();
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for (id_ast, expr_ast) in attrs_ast {
            errs.also(declare_attr(env, directive, &mut prev_attrs, &id_ast));
            match &*id_ast.name {
                "arch" => {
                    attrs.arch = errs.ok(arch_attr(env, directive, expr_ast));
                }
                "charmap" => {
                    attrs.charmap =
                        errs.ok(charmap_attr(env, directive, expr_ast));
                }
                "fill" => {
                    attrs.fill =
                        Some(errs.ok_or_default(fill_attr(
                            env, directive, expr_ast,
                        )))
                }
                _ => {
                    errs.push(AsmError::InvalidAttrName {
                        directive,
                        attr_name: id_ast.name,
                        attr_loc: env.make_loc(id_ast.span),
                    });
                }
            }
        }
        (attrs, errs)
    }
}

//===========================================================================//

fn declare_attr(
    env: &AsmTypeEnv,
    directive: &'static str,
    prev_attrs: &mut HashMap<Rc<str>, SrcSpan>,
    id_ast: &IdentifierAst,
) -> AsmResult<()> {
    if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
        Err(Errs::one(AsmError::DuplicateAttrName {
            directive,
            attr_name: id_ast.name.clone(),
            attr_loc: env.make_loc(id_ast.span),
            prev_loc: env.make_loc(prev_span),
        }))
    } else {
        prev_attrs.insert(id_ast.name.clone(), id_ast.span);
        Ok(())
    }
}

fn align_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    expr_ast: ExprAst,
) -> AsmResult<Align> {
    static_align_attr(env, kind, "align", expr_ast)
}

fn arch_attr(
    env: &AsmTypeEnv,
    directive: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<Rc<str>> {
    let expr_span = expr_ast.span;
    let arch = static_str_attr(env, directive, "arch", expr_ast)?;
    if env.arch_tree().contains_arch(&arch) {
        Ok(arch)
    } else {
        Err(Errs::one(AsmError::UnknownArch {
            arch,
            loc: env.make_loc(expr_span),
        }))
    }
}

fn charmap_attr(
    env: &AsmTypeEnv,
    directive: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<Rc<str>> {
    let expr_span = expr_ast.span;
    let charmap = static_str_attr(env, directive, "charmap", expr_ast)?;
    if env.contains_charmap(&charmap) {
        Ok(charmap)
    } else {
        Err(Errs::one(AsmError::UnknownCharmap {
            charmap,
            loc: env.make_loc(expr_span),
        }))
    }
}

fn fill_attr(
    env: &AsmTypeEnv,
    directive: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<u8> {
    let expr_span = expr_ast.span;
    let bigint = static_int_attr(env, directive, "fill", expr_ast)?;
    u8::try_from(&bigint).map_err(|_| {
        Errs::one(AsmError::DirectiveExprOutOfRange {
            directive,
            component: "fill",
            expr_loc: env.make_loc(expr_span),
            expr_value: bigint,
            valid_range: bigint_range(u8::MIN, u8::MAX),
        })
    })
}

fn start_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    expr_ast: ExprAst,
) -> AsmResult<Addr> {
    let expr_span = expr_ast.span;
    let bigint = static_int_attr(env, kind.directive(), "start", expr_ast)?;
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

fn within_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    expr_ast: ExprAst,
) -> AsmResult<Align> {
    static_align_attr(env, kind, "within", expr_ast)
}

fn static_align_attr(
    env: &AsmTypeEnv,
    kind: AsmChunkKind,
    attr_name: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<Align> {
    let expr_span = expr_ast.span;
    let bigint = static_int_attr(env, kind.directive(), attr_name, expr_ast)?;
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

fn static_int_attr(
    env: &AsmTypeEnv,
    directive: &'static str,
    attr_name: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<BigInt> {
    typecheck_static_dir_expr_as(
        env,
        (directive, attr_name),
        expr_ast,
        ExprType::Integer,
    )
    .map(|value| value.unwrap_int())
}

fn static_str_attr(
    env: &AsmTypeEnv,
    directive: &'static str,
    attr_name: &'static str,
    expr_ast: ExprAst,
) -> AsmResult<Rc<str>> {
    typecheck_static_dir_expr_as(
        env,
        (directive, attr_name),
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
