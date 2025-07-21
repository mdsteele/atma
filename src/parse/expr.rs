//! Facilities for parsing expressions.

use crate::parse::{ParseError, SrcLoc, Token, TokenValue};
use chumsky::{self, Parser};
use num_bigint::BigInt;

//===========================================================================//

/// An abstract syntax tree for an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprAst {
    /// An identifier.
    Identifier(String, SrcLoc),
    /// An integer literal.
    IntLiteral(BigInt),
    /// An addition operation between two subexpressions.
    Plus(Box<ExprAst>, Box<ExprAst>),
}

//===========================================================================//

type Error<'a> = chumsky::extra::Err<chumsky::error::Rich<'a, Token>>;

fn identifier<'a>() -> impl Parser<'a, &'a [Token], ExprAst, Error<'a>> + Clone
{
    chumsky::prelude::any()
        .try_map(|token: Token, span| {
            if let TokenValue::Identifier(id) = token.value {
                Ok(ExprAst::Identifier(id, token.start))
            } else {
                Err(chumsky::error::Rich::custom(span, ""))
            }
        })
        .labelled("identifier")
}

fn int_literal<'a>() -> impl Parser<'a, &'a [Token], ExprAst, Error<'a>> + Clone
{
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

fn symbol<'a>(
    value: TokenValue,
) -> impl Parser<'a, &'a [Token], (), Error<'a>> + Clone {
    let name = value.name();
    chumsky::prelude::any()
        .filter(move |token: &Token| token.value == value)
        .ignored()
        .labelled(name)
}

fn expr<'a>() -> impl Parser<'a, &'a [Token], ExprAst, Error<'a>> {
    chumsky::prelude::recursive(|expr| {
        let expr_atom = chumsky::prelude::choice((
            expr.delimited_by(
                symbol(TokenValue::ParenOpen),
                symbol(TokenValue::ParenClose),
            ),
            identifier(),
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

//===========================================================================//

/// Parses a sequence of tokens into an abstract syntax tree for an expression.
pub fn parse_expr(tokens: &[Token]) -> Result<ExprAst, Vec<ParseError>> {
    expr().parse(tokens).into_result().map_err(|errors| {
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
    use super::{ExprAst, parse_expr};
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
            Ok(ExprAst::Identifier(
                "foo".to_string(),
                SrcLoc { line: 1, column: 0 }
            ))
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
