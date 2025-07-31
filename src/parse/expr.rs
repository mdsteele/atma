//! Facilities for parsing expressions.

use crate::parse::{ParseError, SrcLoc, Token, TokenValue};
use chumsky::{self, Parser};
use num_bigint::BigInt;

//===========================================================================//

/// The error type used for `chumsky::Parser`s in this crate.
pub(crate) type PError<'a> =
    chumsky::extra::Err<chumsky::error::Rich<'a, Token>>;

//===========================================================================//

/// An identifier in an expression or lvalue.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdentifierAst {
    pub id: String,
    pub start: SrcLoc,
}

impl IdentifierAst {
    pub(crate) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], IdentifierAst, PError<'a>> + Clone {
        chumsky::prelude::any()
            .try_map(|token: Token, span| {
                if let TokenValue::Identifier(id) = token.value {
                    Ok(IdentifierAst { id, start: token.start })
                } else {
                    Err(chumsky::error::Rich::custom(span, ""))
                }
            })
            .labelled("identifier")
    }
}

//===========================================================================//

/// An abstract syntax tree for an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprAst {
    /// An identifier.
    Identifier(IdentifierAst),
    /// An integer literal.
    IntLiteral(BigInt),
    /// An addition operation between two subexpressions.
    Plus(Box<ExprAst>, Box<ExprAst>),
}

impl ExprAst {
    pub(crate) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], ExprAst, PError<'a>> + Clone {
        chumsky::prelude::recursive(|expr| {
            let expr_atom = chumsky::prelude::choice((
                expr.delimited_by(
                    symbol(TokenValue::ParenOpen),
                    symbol(TokenValue::ParenClose),
                ),
                IdentifierAst::parser().map(ExprAst::Identifier),
                int_literal(),
            ))
            .labelled("subexpression");

            expr_atom
                .clone()
                .foldl(
                    symbol(TokenValue::Plus).ignore_then(expr_atom).repeated(),
                    |lhs, rhs| ExprAst::Plus(Box::new(lhs), Box::new(rhs)),
                )
                .labelled("expression")
        })
    }
}

//===========================================================================//

fn int_literal<'a>()
-> impl Parser<'a, &'a [Token], ExprAst, PError<'a>> + Clone {
    chumsky::prelude::any()
        .try_map(|token: Token, span| {
            if let TokenValue::IntLiteral(int) = token.value {
                Ok(ExprAst::IntLiteral(int))
            } else {
                Err(chumsky::error::Rich::custom(span, ""))
            }
        })
        .labelled("int literal")
}

pub(crate) fn symbol<'a>(
    value: TokenValue,
) -> impl Parser<'a, &'a [Token], (), PError<'a>> + Clone {
    let name = value.name();
    chumsky::prelude::any()
        .filter(move |token: &Token| token.value == value)
        .ignored()
        .labelled(name)
}

//===========================================================================//

/// Parses a sequence of tokens into an abstract syntax tree for an expression.
pub fn parse_expr(tokens: &[Token]) -> Result<ExprAst, Vec<ParseError>> {
    ExprAst::parser().parse(tokens).into_result().map_err(|errors| {
        errors
            .into_iter()
            .map(|error| {
                let index = error.span().start;
                let location = if index < tokens.len() {
                    tokens[index].start
                } else {
                    tokens[tokens.len() - 1].start
                };
                let message = format!("{error:?}");
                ParseError { location, message }
            })
            .collect()
    })
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ExprAst, IdentifierAst, parse_expr};
    use crate::parse::{ParseError, SrcLoc, Token, TokenLexer};
    use num_bigint::BigInt;

    fn parse(input: &str) -> Result<ExprAst, Vec<ParseError>> {
        parse_expr(
            &TokenLexer::new(input.as_bytes())
                .collect::<Result<Vec<Token>, ParseError>>()
                .map_err(|error| vec![error])?,
        )
    }

    #[test]
    fn identifier() {
        assert_eq!(
            parse("foo"),
            Ok(ExprAst::Identifier(IdentifierAst {
                id: "foo".to_string(),
                start: SrcLoc { line: 1, column: 0 }
            }))
        );
    }

    #[test]
    fn int_literal() {
        assert_eq!(parse("123"), Ok(ExprAst::IntLiteral(BigInt::from(123))));
    }

    #[test]
    fn addition() {
        assert_eq!(
            parse("1 + 2 + (3 + 4)"),
            Ok(ExprAst::Plus(
                Box::new(ExprAst::Plus(
                    Box::new(ExprAst::IntLiteral(BigInt::from(1))),
                    Box::new(ExprAst::IntLiteral(BigInt::from(2))),
                )),
                Box::new(ExprAst::Plus(
                    Box::new(ExprAst::IntLiteral(BigInt::from(3))),
                    Box::new(ExprAst::IntLiteral(BigInt::from(4))),
                )),
            ))
        );
    }
}

//===========================================================================//
