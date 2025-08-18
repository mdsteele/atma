use super::binop::AdsBinOp;
use super::value::{AdsType, AdsValue};
use crate::parse::{
    DeclareAst, ExprAst, ExprAstNode, IdentifierAst, ParseError, SrcSpan,
};
use std::collections::HashMap;

//===========================================================================//

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AdsExprOp {
    BinOp(AdsBinOp),
    Id(String),
    Literal(AdsValue),
}

//===========================================================================//

/// An expression in an [AdsProgram].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AdsExpr {
    pub(crate) ops: Vec<AdsExprOp>,
}

impl AdsExpr {
    pub fn constant<T: Into<AdsValue>>(value: T) -> AdsExpr {
        AdsExpr { ops: vec![AdsExprOp::Literal(value.into())] }
    }

    pub fn typecheck(
        expr: &ExprAst,
        env: &AdsTypeEnv,
    ) -> Result<(AdsExpr, AdsType), Vec<ParseError>> {
        let subexpressions: Vec<&ExprAst> = {
            let mut stack = vec![expr];
            let mut subexprs = Vec::<&ExprAst>::new();
            while let Some(subexpr) = stack.pop() {
                subexprs.push(subexpr);
                match &subexpr.node {
                    ExprAstNode::BinOp(_, lhs, rhs) => {
                        stack.push(lhs);
                        stack.push(rhs);
                    }
                    ExprAstNode::BoolLiteral(_) => {}
                    ExprAstNode::Identifier(_) => {}
                    ExprAstNode::IntLiteral(_) => {}
                    ExprAstNode::StrLiteral(_) => {}
                }
            }
            subexprs
        };
        let mut types = Vec::<AdsType>::new();
        let mut ops = Vec::<AdsExprOp>::new();
        let mut errors = Vec::<ParseError>::new();
        for subexpr in subexpressions.into_iter().rev() {
            match &subexpr.node {
                ExprAstNode::BinOp(binop_ast, lhs_ast, rhs_ast) => {
                    debug_assert!(types.len() >= 2);
                    let rhs_type = types.pop().unwrap();
                    let lhs_type = types.pop().unwrap();
                    match AdsBinOp::typecheck(
                        *binop_ast,
                        lhs_ast.span,
                        lhs_type,
                        rhs_ast.span,
                        rhs_type,
                    ) {
                        Ok((binop, ty)) => {
                            ops.push(AdsExprOp::BinOp(binop));
                            types.push(ty);
                        }
                        Err(mut errs) => {
                            errors.append(&mut errs);
                            types.push(AdsType::Bottom);
                        }
                    }
                }
                &ExprAstNode::BoolLiteral(value) => {
                    ops.push(AdsExprOp::Literal(AdsValue::Boolean(value)));
                    types.push(AdsType::Boolean);
                }
                ExprAstNode::Identifier(id) => {
                    if let Some(ty) = env.get_type(id) {
                        ops.push(AdsExprOp::Id(id.clone()));
                        types.push(ty.clone());
                    } else {
                        let span = subexpr.span;
                        let message = format!("No such identifier: {}", id);
                        errors.push(ParseError::new(span, message));
                        types.push(AdsType::Bottom);
                    }
                }
                ExprAstNode::IntLiteral(value) => {
                    ops.push(AdsExprOp::Literal(AdsValue::Integer(
                        value.clone(),
                    )));
                    types.push(AdsType::Integer);
                }
                ExprAstNode::StrLiteral(value) => {
                    ops.push(AdsExprOp::Literal(AdsValue::String(
                        value.clone(),
                    )));
                    types.push(AdsType::String);
                }
            }
        }
        debug_assert_eq!(types.len(), 1);
        if errors.is_empty() {
            Ok((AdsExpr { ops }, types.pop().unwrap()))
        } else {
            Err(errors)
        }
    }
}

//===========================================================================//

pub struct AdsTypeEnv {
    variables: HashMap<String, (DeclareAst, SrcSpan, AdsType)>,
}

impl AdsTypeEnv {
    pub fn empty() -> AdsTypeEnv {
        AdsTypeEnv { variables: HashMap::new() }
    }

    pub fn declare(
        &mut self,
        kind: DeclareAst,
        id: IdentifierAst,
        ty: AdsType,
    ) {
        self.variables.insert(id.name, (kind, id.span, ty));
    }

    pub fn get_declaration(
        &self,
        id: &str,
    ) -> Option<(DeclareAst, SrcSpan, &AdsType)> {
        self.variables.get(id).map(|(kind, span, ty)| (*kind, *span, ty))
    }

    pub fn get_type(&self, id: &str) -> Option<&AdsType> {
        self.variables.get(id).map(|(_, _, ty)| ty)
    }
}

//===========================================================================//
