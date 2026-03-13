use super::lex::{Token, TokenLexer, TokenValue};
use super::types::{ParseError, ParseResult};
use chumsky::{self, Parser};

//===========================================================================//

/// The error type used for `chumsky::Parser`s in this crate.
pub(super) type Extra<'a> =
    chumsky::extra::Full<chumsky::error::Rich<'a, Token>, (), Context>;

#[derive(Clone, Copy, Default)]
pub(super) struct Context {
    pub allow_placeholder_as_identifier: bool,
}

//===========================================================================//

pub(super) fn tokenize(source: &str) -> ParseResult<Vec<Token>> {
    let lexer = TokenLexer::new(source);
    lexer.collect::<Result<_, _>>().map_err(|error| vec![error])
}

pub(super) fn parse_tokens<'a, T, P>(
    parser: P,
    tokens: &'a [Token],
) -> ParseResult<T>
where
    P: Parser<'a, &'a [Token], T, Extra<'a>>,
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

pub(super) fn directive<'a>(
    word: &'static str,
) -> impl Parser<'a, &'a [Token], (), Extra<'a>> + Clone {
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

pub(super) fn keyword<'a>(
    word: &'static str,
) -> impl Parser<'a, &'a [Token], (), Extra<'a>> + Clone {
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

pub(super) fn linebreak<'a>()
-> impl Parser<'a, &'a [Token], (), Extra<'a>> + Clone {
    symbol(TokenValue::Linebreak).repeated().at_least(1)
}

pub(super) fn symbol<'a>(
    value: TokenValue,
) -> impl Parser<'a, &'a [Token], Token, Extra<'a>> + Clone {
    let name = value.name();
    chumsky::prelude::any()
        .filter(move |token: &Token| token.value == value)
        .labelled(name)
}

//===========================================================================//
