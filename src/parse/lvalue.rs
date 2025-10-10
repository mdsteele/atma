use super::atom::{PError, symbol};
use super::expr::{ExprAst, IdentifierAst};
use super::lex::{Token, TokenValue};
use super::types::SrcSpan;
use chumsky::{self, IterParser, Parser};

//===========================================================================//

/// The abstract syntax tree for an L-value.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LValueAst {
    /// The location in the source code where this L-value appears.
    pub span: SrcSpan,
    /// The contents of this L-value.
    pub node: LValueAstNode,
}

impl LValueAst {
    pub(crate) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], LValueAst, PError<'a>> + Clone {
        chumsky::prelude::recursive(|lvalue| {
            let memory = chumsky::prelude::group((
                symbol(TokenValue::BracketOpen),
                ExprAst::parser(),
                symbol(TokenValue::BracketClose),
            ))
            .map(
                |(open, expr, close): (Token, ExprAst, Token)| LValueAst {
                    span: open.span.merged_with(close.span),
                    node: LValueAstNode::Memory(expr),
                },
            );
            let tuple = chumsky::prelude::group((
                symbol(TokenValue::ParenOpen),
                lvalue
                    .separated_by(symbol(TokenValue::Comma))
                    .at_least(2)
                    .collect::<Vec<_>>(),
                symbol(TokenValue::ParenClose),
            ))
            .map(
                |(open, asts, close): (Token, Vec<LValueAst>, Token)| {
                    LValueAst {
                        span: open.span.merged_with(close.span),
                        node: LValueAstNode::Tuple(asts),
                    }
                },
            );
            let variable = IdentifierAst::parser().map(|id_ast| LValueAst {
                span: id_ast.span,
                node: LValueAstNode::Variable(id_ast.name),
            });
            chumsky::prelude::choice((memory, tuple, variable))
        })
    }
}

//===========================================================================//

/// One node in the abstract syntax tree for an L-value.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LValueAstNode {
    /// Assign to one byte of memory in the simulated memory bus, at the
    /// address given by the specified expression.
    Memory(ExprAst),
    /// Assign to a tuple of L-values.
    Tuple(Vec<LValueAst>),
    /// Assign to a variable (or simulated processor register or PC).
    Variable(String),
}

//===========================================================================//
