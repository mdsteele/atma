//! Facilities for parsing expressions.

use crate::parse::{ParseError, SrcSpan, Token, TokenValue};
use chumsky::{self, IterParser, Parser};
use num_bigint::BigInt;

//===========================================================================//

/// The error type used for `chumsky::Parser`s in this crate.
pub(crate) type PError<'a> =
    chumsky::extra::Err<chumsky::error::Rich<'a, Token>>;

//===========================================================================//

/// A binary operation between two expressions in an abstract syntax tree.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinOpAst {
    /// "Equals" comparison.
    CmpEq,
    /// Addition.
    Plus,
}

impl BinOpAst {
    pub(crate) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], (SrcSpan, BinOpAst), PError<'a>> + Clone
    {
        chumsky::prelude::any()
            .try_map(|token: Token, span| {
                let op = match token.value {
                    TokenValue::EqEq => BinOpAst::CmpEq,
                    TokenValue::Plus => BinOpAst::Plus,
                    _ => return Err(chumsky::error::Rich::custom(span, "")),
                };
                Ok((token.span, op))
            })
            .labelled("binary operator")
    }

    pub(crate) fn verb(self) -> &'static str {
        match self {
            BinOpAst::CmpEq => "compare",
            BinOpAst::Plus => "add",
        }
    }
}

//===========================================================================//

/// An identifier in an expression or lvalue.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdentifierAst {
    /// The location in the source code where this instance of the identifier
    /// appears.
    pub span: SrcSpan,
    /// The name of the identifier.
    pub name: String,
}

impl IdentifierAst {
    pub(crate) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], IdentifierAst, PError<'a>> + Clone {
        chumsky::prelude::any()
            .try_map(|token: Token, span| {
                if let TokenValue::Identifier(name) = token.value {
                    Ok(IdentifierAst { name, span: token.span })
                } else {
                    Err(chumsky::error::Rich::custom(span, ""))
                }
            })
            .labelled("identifier")
    }
}

//===========================================================================//

/// The abstract syntax tree for an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExprAst {
    /// The location in the source code where this expression appears.
    pub span: SrcSpan,
    /// The contents of this expression.
    pub node: ExprAstNode,
}

impl ExprAst {
    pub(crate) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], ExprAst, PError<'a>> + Clone {
        chumsky::prelude::recursive(|expr| {
            let parenthesized_expr = chumsky::prelude::group((
                symbol(TokenValue::ParenOpen),
                expr.clone()
                    .separated_by(symbol(TokenValue::Comma))
                    .collect::<Vec<_>>(),
                symbol(TokenValue::ParenClose),
            ))
            .map(
                |(open, mut asts, close): (Token, Vec<ExprAst>, Token)| {
                    ExprAst {
                        span: open.span.merged_with(close.span),
                        node: if asts.len() == 1 {
                            asts.pop().unwrap().node
                        } else {
                            ExprAstNode::TupleLiteral(asts)
                        },
                    }
                },
            );
            let list_literal = chumsky::prelude::group((
                symbol(TokenValue::BraceOpen),
                expr.separated_by(symbol(TokenValue::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
                symbol(TokenValue::BraceClose),
            ))
            .map(
                |(open, asts, close): (Token, Vec<ExprAst>, Token)| ExprAst {
                    span: open.span.merged_with(close.span),
                    node: ExprAstNode::ListLiteral(asts),
                },
            );
            let identifier = IdentifierAst::parser().map(|id| ExprAst {
                span: id.span,
                node: ExprAstNode::Identifier(id.name),
            });

            let expr_atom = chumsky::prelude::choice((
                parenthesized_expr,
                identifier,
                list_literal,
                bool_literal(),
                int_literal(),
                str_literal(),
            ))
            .labelled("subexpression");

            expr_atom
                .clone()
                .foldl(
                    BinOpAst::parser().then(expr_atom).repeated(),
                    |lhs, (op, rhs)| {
                        let span = lhs.span.merged_with(rhs.span);
                        let node = ExprAstNode::BinOp(
                            op,
                            Box::new(lhs),
                            Box::new(rhs),
                        );
                        ExprAst { span, node }
                    },
                )
                .labelled("expression")
        })
    }
}

//===========================================================================//

/// One node in the abstract syntax tree for an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprAstNode {
    /// A binary operation between two subexpressions.
    BinOp((SrcSpan, BinOpAst), Box<ExprAst>, Box<ExprAst>),
    /// An boolean literal.
    BoolLiteral(bool),
    /// An identifier.
    Identifier(String),
    /// An integer literal.
    IntLiteral(BigInt),
    /// A list literal.
    ListLiteral(Vec<ExprAst>),
    /// A string literal.
    StrLiteral(String),
    /// A tuple literal.
    TupleLiteral(Vec<ExprAst>),
}

//===========================================================================//

fn bool_literal<'a>()
-> impl Parser<'a, &'a [Token], ExprAst, PError<'a>> + Clone {
    chumsky::prelude::any()
        .try_map(|token: Token, span| {
            if let TokenValue::BoolLiteral(boolean) = token.value {
                Ok(ExprAst {
                    span: token.span,
                    node: ExprAstNode::BoolLiteral(boolean),
                })
            } else {
                Err(chumsky::error::Rich::custom(span, ""))
            }
        })
        .labelled("boolean literal")
}

fn int_literal<'a>()
-> impl Parser<'a, &'a [Token], ExprAst, PError<'a>> + Clone {
    chumsky::prelude::any()
        .try_map(|token: Token, span| {
            if let TokenValue::IntLiteral(int) = token.value {
                Ok(ExprAst {
                    span: token.span,
                    node: ExprAstNode::IntLiteral(int),
                })
            } else {
                Err(chumsky::error::Rich::custom(span, ""))
            }
        })
        .labelled("integer literal")
}

fn str_literal<'a>()
-> impl Parser<'a, &'a [Token], ExprAst, PError<'a>> + Clone {
    chumsky::prelude::any()
        .try_map(|token: Token, span| {
            if let TokenValue::StrLiteral(string) = token.value {
                Ok(ExprAst {
                    span: token.span,
                    node: ExprAstNode::StrLiteral(string),
                })
            } else {
                Err(chumsky::error::Rich::custom(span, ""))
            }
        })
        .labelled("string literal")
}

pub(crate) fn symbol<'a>(
    value: TokenValue,
) -> impl Parser<'a, &'a [Token], Token, PError<'a>> + Clone {
    let name = value.name();
    chumsky::prelude::any()
        .filter(move |token: &Token| token.value == value)
        .labelled(name)
}

//===========================================================================//

/// Parses a sequence of tokens into an abstract syntax tree for an expression.
pub fn parse_expr(tokens: &[Token]) -> Result<ExprAst, Vec<ParseError>> {
    assert!(!tokens.is_empty());
    ExprAst::parser().parse(tokens).into_result().map_err(|errors| {
        errors
            .into_iter()
            .map(|error| {
                let index = error.span().start;
                let span = if index < tokens.len() {
                    tokens[index].span
                } else {
                    tokens[tokens.len() - 1].span.end_span()
                };
                ParseError::new(span, format!("{error:?}"))
            })
            .collect()
    })
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{BinOpAst, ExprAst, ExprAstNode, parse_expr};
    use crate::parse::{ParseError, SrcSpan, Token, TokenLexer};
    use num_bigint::BigInt;
    use std::ops::Range;

    fn parse(input: &str) -> Result<ExprAst, Vec<ParseError>> {
        parse_expr(
            &TokenLexer::new(input)
                .collect::<Result<Vec<Token>, ParseError>>()
                .map_err(|error| vec![error])?,
        )
    }

    fn int_node(value: i32) -> ExprAstNode {
        ExprAstNode::IntLiteral(BigInt::from(value))
    }

    fn int_ast(range: Range<usize>, value: i32) -> ExprAst {
        ExprAst {
            span: SrcSpan::from_byte_range(range),
            node: int_node(value),
        }
    }

    #[test]
    fn identifier() {
        assert_eq!(
            parse("foo"),
            Ok(ExprAst {
                span: SrcSpan::from_byte_range(0..3),
                node: ExprAstNode::Identifier("foo".to_string()),
            })
        );
    }

    #[test]
    fn int_literal() {
        assert_eq!(parse("123"), Ok(int_ast(0..3, 123)));
    }

    #[test]
    fn addition() {
        assert_eq!(
            parse("1 + 2 + (3 + 4)"),
            Ok(ExprAst {
                span: SrcSpan::from_byte_range(0..15),
                node: ExprAstNode::BinOp(
                    (SrcSpan::from_byte_range(6..7), BinOpAst::Plus),
                    Box::new(ExprAst {
                        span: SrcSpan::from_byte_range(0..5),
                        node: ExprAstNode::BinOp(
                            (SrcSpan::from_byte_range(2..3), BinOpAst::Plus),
                            Box::new(int_ast(0..1, 1)),
                            Box::new(int_ast(4..5, 2)),
                        ),
                    }),
                    Box::new(ExprAst {
                        span: SrcSpan::from_byte_range(8..15),
                        node: ExprAstNode::BinOp(
                            (SrcSpan::from_byte_range(11..12), BinOpAst::Plus),
                            Box::new(int_ast(9..10, 3)),
                            Box::new(int_ast(13..14, 4)),
                        ),
                    }),
                ),
            })
        );
    }
}

//===========================================================================//
