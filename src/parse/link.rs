//! Facilities for parsing linker configuration files.

use super::atom::{
    PError, directive, linebreak, parse_tokens, symbol, tokenize,
};
use super::expr::{ExprAst, IdentifierAst};
use super::lex::{Token, TokenValue};
use super::types::ParseResult;
use chumsky::{self, IterParser, Parser};

//===========================================================================//

/// The abstract syntax tree for a linker configuration file.
#[derive(Debug)]
pub struct LinkConfigAst {
    /// Top-level directives in the configuration file.
    pub directives: Vec<LinkDirectiveAst>,
}

impl LinkConfigAst {
    /// Parses a linker configuration.
    pub fn parse_source(source: &str) -> ParseResult<LinkConfigAst> {
        let tokens = tokenize(source)?;
        parse_tokens(LinkConfigAst::parser(), &tokens)
    }

    fn parser<'a>() -> impl Parser<'a, &'a [Token], LinkConfigAst, PError<'a>>
    {
        symbol(TokenValue::Linebreak)
            .repeated()
            .ignore_then(
                LinkDirectiveAst::parser().repeated().collect::<Vec<_>>(),
            )
            .map(|directives| LinkConfigAst { directives })
    }
}

//===========================================================================//

/// The abstract syntax tree for a single directive in a linker configuration
/// file.
#[derive(Debug)]
pub enum LinkDirectiveAst {
    /// An `.ADDRSPACES` directive block.
    Addrspaces(Vec<LinkEntryAst>),
    /// A `.LET` directive.
    Let(IdentifierAst, ExprAst),
    /// A `.MEMORY` directive block.
    Memory(Vec<LinkEntryAst>),
    /// A `.SECTIONS` directive block.
    Sections(Vec<LinkEntryAst>),
}

impl LinkDirectiveAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], LinkDirectiveAst, PError<'a>> {
        let entries_block = symbol(TokenValue::BraceOpen)
            .ignore_then(linebreak())
            .ignore_then(LinkEntryAst::parser().repeated().collect::<Vec<_>>())
            .then_ignore(symbol(TokenValue::BraceClose))
            .then_ignore(linebreak());
        let addrspaces_dir = directive(".ADDRSPACES")
            .ignore_then(entries_block.clone())
            .map(LinkDirectiveAst::Addrspaces);
        let let_dir = directive(".LET")
            .ignore_then(IdentifierAst::parser())
            .then_ignore(symbol(TokenValue::Equals))
            .then(ExprAst::parser())
            .then_ignore(linebreak())
            .map(|(id, expr)| LinkDirectiveAst::Let(id, expr));
        let memory_dir = directive(".MEMORY")
            .ignore_then(entries_block.clone())
            .map(LinkDirectiveAst::Memory);
        let sections_dir = directive(".SECTIONS")
            .ignore_then(entries_block)
            .map(LinkDirectiveAst::Sections);
        chumsky::prelude::choice((
            addrspaces_dir,
            let_dir,
            memory_dir,
            sections_dir,
        ))
    }
}

//===========================================================================//

/// The abstract syntax tree for one entry in a linker configuration directive
/// block.
#[derive(Debug)]
pub struct LinkEntryAst {
    /// The name of this entry.
    pub id: IdentifierAst,
    /// Key/value attributes associated with this entry.
    pub attrs: Vec<(IdentifierAst, ExprAst)>,
}

impl LinkEntryAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], LinkEntryAst, PError<'a>> + Clone {
        let attribute = IdentifierAst::parser()
            .then_ignore(symbol(TokenValue::Equals))
            .then(ExprAst::parser());
        IdentifierAst::parser()
            .then_ignore(symbol(TokenValue::Colon))
            .then(
                attribute
                    .separated_by(symbol(TokenValue::Comma))
                    .collect::<Vec<_>>(),
            )
            .then_ignore(linebreak())
            .map(|(id, attrs)| LinkEntryAst { id, attrs })
    }
}

//===========================================================================//
