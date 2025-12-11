use super::lex::{Token, TokenLexer, TokenValue};
use super::types::{ParseError, ParseResult};
use chumsky::{self, Parser};

//===========================================================================//

/// The error type used for `chumsky::Parser`s in this crate.
pub(crate) type PError<'a> =
    chumsky::extra::Err<chumsky::error::Rich<'a, Token>>;

//===========================================================================//

pub(crate) fn tokenize(source: &str) -> ParseResult<Vec<Token>> {
    let lexer = TokenLexer::new(source);
    lexer.collect::<Result<_, _>>().map_err(|error| vec![error])
}

pub(crate) fn parse_tokens<'a, T, P>(
    parser: P,
    tokens: &'a [Token],
) -> ParseResult<T>
where
    P: Parser<'a, &'a [Token], T, PError<'a>>,
{
    parser.parse(tokens).into_result().map_err(|errors| {
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

pub(crate) fn directive<'a>(
    word: &'static str,
) -> impl Parser<'a, &'a [Token], (), PError<'a>> + Clone {
    chumsky::prelude::any()
        .filter(move |token: &Token| {
            if let TokenValue::Directive(id) = &token.value {
                id.eq_ignore_ascii_case(word)
            } else {
                false
            }
        })
        .ignored()
        .labelled(word)
}

pub(crate) fn keyword<'a>(
    word: &'static str,
) -> impl Parser<'a, &'a [Token], (), PError<'a>> + Clone {
    chumsky::prelude::any()
        .filter(move |token: &Token| {
            if let TokenValue::Identifier(id) = &token.value {
                **id == *word
            } else {
                false
            }
        })
        .ignored()
        .labelled(word)
}

pub(crate) fn linebreak<'a>()
-> impl Parser<'a, &'a [Token], (), PError<'a>> + Clone {
    symbol(TokenValue::Linebreak).repeated().at_least(1)
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
