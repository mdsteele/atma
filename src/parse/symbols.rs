use super::atom::{AtmaParser, linebreak, parse_tokens, symbol, tokenize};
use super::error::ParseResult;
use crate::addr::Addr;
use crate::lex::{Token, TokenValue};
use chumsky::{self, IterParser, Parser};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::rc::Rc;

//===========================================================================//

/// A symbol table.
#[derive(Default)]
pub struct SymbolTable {
    /// An entry for each symbol in the table.
    pub entries: Vec<SymbolEntry>,
}

impl SymbolTable {
    /// Reads a symbol table from a file.
    pub fn parse_source(source: &str) -> ParseResult<Self> {
        let tokens = tokenize(source)?;
        Self::parse(&tokens)
    }

    /// Parses a sequence of tokens into a symbol table.
    pub fn parse(tokens: &[Token]) -> ParseResult<Self> {
        parse_tokens(Self::parser(), tokens)
    }

    pub(super) fn parser<'a>() -> impl AtmaParser<'a, Self> {
        SymbolEntry::parser()
            .repeated()
            .collect::<Vec<_>>()
            .map(|entries| Self { entries })
    }
}

//===========================================================================//

/// One entry in a symbol table.
pub struct SymbolEntry {
    /// The byte offset into the binary for the symbol, or `None` if the symbol
    /// doesn't have a meaningful location in the binary.
    pub binary_offset: Option<u64>,
    /// The name of the address space that the symbol exists in.
    pub space: Rc<str>,
    /// The address of the symbol within its address space.
    pub address: Addr,
    /// True if this symbol is external.  A linked binary will not contain two
    /// external symbols with the same `name`.
    pub is_external: bool,
    /// The fully qualified name of the symbol.
    ///
    /// Note that a linked binary can contain multiple symbols with the same
    /// name, but `external` will be true for at most one of those symbols.
    pub name: Rc<str>,
}

impl SymbolEntry {
    pub(super) fn parser<'a>() -> impl AtmaParser<'a, Self> {
        chumsky::prelude::group((
            u64_literal().or_not().then_ignore(symbol(TokenValue::Or)),
            identifier().then_ignore(symbol(TokenValue::Or)),
            addr_literal().then_ignore(symbol(TokenValue::Or)),
            chumsky::prelude::choice((
                symbol(TokenValue::Plus).to(true),
                symbol(TokenValue::Minus).to(false),
            )),
            compound_id().then_ignore(linebreak()),
        ))
        .map(|(binary_offset, space, address, is_external, name)| {
            Self { binary_offset, space, address, is_external, name }
        })
    }
}

//===========================================================================//

fn addr_literal<'a>() -> impl AtmaParser<'a, Addr> {
    int_literal().try_map(|bigint: BigInt, span| {
        Addr::try_from(&bigint)
            .map_err(|()| chumsky::error::Rich::custom(span, ""))
    })
}

fn u64_literal<'a>() -> impl AtmaParser<'a, u64> {
    int_literal().try_map(|bigint: BigInt, span| {
        bigint.to_u64().ok_or_else(|| chumsky::error::Rich::custom(span, ""))
    })
}

fn int_literal<'a>() -> impl AtmaParser<'a, BigInt> {
    chumsky::prelude::any()
        .try_map(|token: Token, span| match token.value {
            TokenValue::IntLiteral(bigint) => Ok(bigint),
            _ => Err(chumsky::error::Rich::custom(span, "")),
        })
        .labelled("integer literal")
}

fn identifier<'a>() -> impl AtmaParser<'a, Rc<str>> {
    chumsky::prelude::any()
        .try_map(|token: Token, span| match token.value {
            TokenValue::Identifier(name) => Ok(name),
            _ => Err(chumsky::error::Rich::custom(span, "")),
        })
        .labelled("identifier")
}

fn compound_id<'a>() -> impl AtmaParser<'a, Rc<str>> {
    identifier()
        .or(int_literal().map(|bigint| Rc::from(format!("${bigint:x}"))))
        .separated_by(symbol(TokenValue::ColonColon))
        .collect::<Vec<_>>()
        .map(|parts| Rc::<str>::from(parts.join("::")))
}

//===========================================================================//
