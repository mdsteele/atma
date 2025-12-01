use super::lex::{Token, TokenValue};
use chumsky::{self, Parser};

//===========================================================================//

/// The error type used for `chumsky::Parser`s in this crate.
pub(crate) type PError<'a> =
    chumsky::extra::Err<chumsky::error::Rich<'a, Token>>;

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
