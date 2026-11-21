use super::atom::{AtmaParser, Language, symbol};
use super::expr::{BinOpAst, ExprAst};
use super::id::CompoundIdAst;
use crate::error::SrcSpan;
use crate::lex::{Token, TokenValue};
use chumsky::{self, ConfigParser, IterParser, Parser};

//===========================================================================//

/// The abstract syntax tree for an assignment operator.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AssignOpAst {
    /// The location in the source code where this operator appears.
    pub span: SrcSpan,
    /// What kind of assignment operator this is.
    pub kind: AssignOpKind,
}

impl AssignOpAst {
    pub(super) fn parser<'a>() -> impl AtmaParser<'a, Self> {
        AssignOpKind::parser().map(|(span, kind)| Self { span, kind })
    }
}

//===========================================================================//

/// An assignment operator in an abstract syntax tree.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AssignOpKind {
    /// Assign X = Y.
    Equals,
    /// Assign X = X op Y.
    OpEquals(BinOpAst),
}

impl AssignOpKind {
    fn parser<'a>() -> impl AtmaParser<'a, (SrcSpan, Self)> {
        chumsky::prelude::choice((
            symbol(TokenValue::Equals)
                .map(|token| (token.span, AssignOpKind::Equals)),
            symbol(TokenValue::AndEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::BitAnd))
            }),
            symbol(TokenValue::CaretEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::BitXor))
            }),
            symbol(TokenValue::GreaterGreaterEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Shr))
            }),
            symbol(TokenValue::LessLessEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Shl))
            }),
            symbol(TokenValue::MinusEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Sub))
            }),
            symbol(TokenValue::OrEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::BitOr))
            }),
            symbol(TokenValue::PercentEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Mod))
            }),
            symbol(TokenValue::PlusEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Add))
            }),
            symbol(TokenValue::PlusPlusEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Concat))
            }),
            symbol(TokenValue::SlashEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Div))
            }),
            symbol(TokenValue::StarEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Mul))
            }),
            symbol(TokenValue::StarStarEquals).map(|token| {
                (token.span, AssignOpKind::OpEquals(BinOpAst::Pow))
            }),
        ))
    }
}

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
    pub(super) fn parser<'a>() -> impl AtmaParser<'a, Self> {
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
            let variable = CompoundIdAst::parser().map(|compound| LValueAst {
                span: compound.span,
                node: LValueAstNode::Variable(compound),
            });
            let wildcard = symbol(TokenValue::Underscore).map(|token| {
                LValueAst { span: token.span, node: LValueAstNode::Wildcard }
            });
            chumsky::prelude::choice((
                memory
                    .contextual()
                    .configure(|_, lang| matches!(lang, Language::Debugger)),
                tuple,
                variable,
                wildcard,
            ))
        })
    }
}

//===========================================================================//

/// One node in the abstract syntax tree for an L-value.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LValueAstNode {
    /// Assign to one byte of memory in the simulated memory bus, at the
    /// address given by the specified expression.  These will only parse when
    /// the `Language` is `Debugger`.
    Memory(ExprAst),
    /// Assign to a tuple of L-values.
    Tuple(Vec<LValueAst>),
    /// Assign to a variable (or to a simulated processor register or PC, in
    /// debugger scripts).
    Variable(CompoundIdAst),
    // TODO: Allow list/tuple index assignment as an lvalue node
    /// Ignore the expression.
    ///
    /// This is primarily useful when used as one element of a `Tuple` L-value.
    Wildcard,
}

//===========================================================================//
