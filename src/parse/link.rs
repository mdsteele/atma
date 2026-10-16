//! Facilities for parsing linker configuration files.

use super::atom::{
    AtmaParser, Language, directive, linebreak, parse_tokens, symbol, tokenize,
};
use super::attr::AttributeAst;
use super::error::ParseResult;
use super::expr::ExprAst;
use super::id::IdentifierAst;
use crate::lex::TokenValue;
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

    fn parser<'a>() -> impl AtmaParser<'a, Self> {
        symbol(TokenValue::Linebreak)
            .repeated()
            .ignore_then(
                LinkDirectiveAst::parser().repeated().collect::<Vec<_>>(),
            )
            .map(|directives| LinkConfigAst { directives })
            .with_ctx(Language::Linker)
    }
}

//===========================================================================//

/// The abstract syntax tree for a single directive in a linker configuration
/// file.
#[derive(Debug)]
pub enum LinkDirectiveAst {
    /// An `.ADDRSPACES` directive block.
    Addrspaces(LinkBlockAst),
    /// A `.CHECKSUMS` directive block.
    Checksums(LinkBlockAst),
    /// An `.EXPORTS` directive block.
    Exports(LinkBlockAst),
    /// An `.IMPORT` directive.
    Import(Vec<IdentifierAst>),
    /// A `.LET` directive.
    Let(IdentifierAst, ExprAst),
    /// A `.REGIONS` directive block.
    Regions(LinkBlockAst),
    /// A `.SECTIONS` directive block.
    Sections(LinkBlockAst),
    /// A `.USE` directive.
    Use(ExprAst),
}

impl LinkDirectiveAst {
    fn parser<'a>() -> impl AtmaParser<'a, Self> {
        let addrspaces_dir = directive(LinkBlockAst::ADDRSPACES_DIRECTIVE)
            .ignore_then(LinkBlockAst::parser())
            .map(LinkDirectiveAst::Addrspaces);
        let checksums_dir = directive(LinkBlockAst::CHECKSUMS_DIRECTIVE)
            .ignore_then(LinkBlockAst::parser())
            .map(LinkDirectiveAst::Checksums);
        let exports_dir = directive(LinkBlockAst::EXPORTS_DIRECTIVE)
            .ignore_then(LinkBlockAst::parser())
            .map(LinkDirectiveAst::Exports);
        let import_dir = directive(".IMPORT")
            .ignore_then(
                IdentifierAst::parser()
                    .separated_by(symbol(TokenValue::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(linebreak())
            .map(LinkDirectiveAst::Import);
        let let_dir = directive(".LET")
            .ignore_then(IdentifierAst::parser())
            .then_ignore(symbol(TokenValue::Equals))
            .then(ExprAst::parser())
            .then_ignore(linebreak())
            .map(|(id, expr)| LinkDirectiveAst::Let(id, expr));
        let regions_dir = directive(LinkBlockAst::REGIONS_DIRECTIVE)
            .ignore_then(LinkBlockAst::parser())
            .map(LinkDirectiveAst::Regions);
        let sections_dir = directive(LinkBlockAst::SECTIONS_DIRECTIVE)
            .ignore_then(LinkBlockAst::parser())
            .map(LinkDirectiveAst::Sections);
        let use_dir = directive(".USE")
            .ignore_then(ExprAst::parser())
            .then_ignore(linebreak())
            .map(LinkDirectiveAst::Use);
        chumsky::prelude::choice((
            addrspaces_dir,
            checksums_dir,
            exports_dir,
            import_dir,
            let_dir,
            regions_dir,
            sections_dir,
            use_dir,
        ))
    }
}

//===========================================================================//

/// The abstract syntax tree for a linker configuration directive block.
#[derive(Debug)]
pub struct LinkBlockAst {
    /// Key/value attributes used as defaults for all entries in this block.
    pub base_attrs: Vec<AttributeAst>,
    /// The entries in this block.
    pub entries: Vec<LinkEntryAst>,
}

impl LinkBlockAst {
    pub(crate) const ADDRSPACES_DIRECTIVE: &str = ".ADDRSPACES";
    pub(crate) const CHECKSUMS_DIRECTIVE: &str = ".CHECKSUMS";
    pub(crate) const EXPORTS_DIRECTIVE: &str = ".EXPORTS";
    pub(crate) const REGIONS_DIRECTIVE: &str = ".REGIONS";
    pub(crate) const SECTIONS_DIRECTIVE: &str = ".SECTIONS";

    fn parser<'a>() -> impl AtmaParser<'a, Self> {
        attributes()
            .then_ignore(symbol(TokenValue::BraceOpen))
            .then_ignore(linebreak())
            .then(LinkEntryAst::parser().repeated().collect::<Vec<_>>())
            .then_ignore(symbol(TokenValue::BraceClose))
            .then_ignore(linebreak())
            .map(|(base_attrs, entries)| Self { base_attrs, entries })
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
    pub attrs: Vec<AttributeAst>,
}

impl LinkEntryAst {
    fn parser<'a>() -> impl AtmaParser<'a, Self> {
        IdentifierAst::parser()
            .then_ignore(symbol(TokenValue::Colon))
            .then(attributes())
            .then_ignore(linebreak())
            .map(|(id, attrs)| LinkEntryAst { id, attrs })
    }
}

//===========================================================================//

fn attributes<'a>() -> impl AtmaParser<'a, Vec<AttributeAst>> {
    AttributeAst::parser()
        .separated_by(symbol(TokenValue::Comma))
        .collect::<Vec<_>>()
}

//===========================================================================//
