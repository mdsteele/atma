//! Facilities for parsing assembly source code.

use super::atom::{
    PError, directive, linebreak, parse_tokens, symbol, tokenize,
};
use super::expr::{ExprAst, IdentifierAst};
use super::lex::{Token, TokenValue};
use super::types::ParseResult;
use chumsky::{self, IterParser, Parser};

//===========================================================================//

/// The abstract syntax tree for an assembly file.
#[derive(Debug)]
pub struct AsmModuleAst {
    /// The statements in this file.
    pub statements: Vec<AsmStmtAst>,
}

impl AsmModuleAst {
    /// Parses assembly source code.
    pub fn parse_source(source: &str) -> ParseResult<AsmModuleAst> {
        let tokens = tokenize(source)?;
        let parser = symbol(TokenValue::Linebreak)
            .repeated()
            .ignore_then(AsmStmtAst::parser())
            .repeated()
            .collect::<Vec<_>>()
            .map(|statements| AsmModuleAst { statements });
        parse_tokens(parser, &tokens)
    }
}

//===========================================================================//

/// The abstract syntax tree for a single statement or declaration in an
/// assembly file.
#[derive(Clone, Debug)]
pub enum AsmStmtAst {
    /// An `.IMPORT` directive.
    Import(IdentifierAst),
    /// A macro invocation.
    Invoke(AsmInvokeAst),
    /// A label.
    Label(IdentifierAst),
    /// A `.SECTION` block.
    Section(AsmSectionAst),
    /// A `.U8` directive.
    U8(ExprAst),
    /// A `.U16LE` directive.
    U16le(ExprAst),
    /// A `.U24LE` directive.
    U24le(ExprAst),
}

impl AsmStmtAst {
    fn parser<'a>() -> impl Parser<'a, &'a [Token], AsmStmtAst, PError<'a>> {
        chumsky::prelude::recursive(|statement| {
            let label = IdentifierAst::parser()
                .then_ignore(symbol(TokenValue::Colon))
                .then_ignore(symbol(TokenValue::Linebreak).repeated())
                .map(AsmStmtAst::Label);
            let stmt_block = symbol(TokenValue::BraceOpen)
                .ignore_then(linebreak())
                .ignore_then(statement.repeated().collect::<Vec<_>>())
                .then_ignore(symbol(TokenValue::BraceClose));
            let import_dir = directive(".IMPORT")
                .ignore_then(IdentifierAst::parser())
                .then_ignore(linebreak())
                .map(AsmStmtAst::Import);
            let section_dir = directive(".SECTION")
                .ignore_then(ExprAst::parser())
                .then(stmt_block)
                .then_ignore(linebreak())
                .map(|(name, body)| {
                    AsmStmtAst::Section(AsmSectionAst { name, body })
                });
            let u8_dir = directive(".U8")
                .ignore_then(ExprAst::parser())
                .then_ignore(linebreak())
                .map(AsmStmtAst::U8);
            let u16le_dir = directive(".U16LE")
                .ignore_then(ExprAst::parser())
                .then_ignore(linebreak())
                .map(AsmStmtAst::U16le);
            let u24le_dir = directive(".U24LE")
                .ignore_then(ExprAst::parser())
                .then_ignore(linebreak())
                .map(AsmStmtAst::U24le);
            chumsky::prelude::choice((
                label,
                import_dir,
                section_dir,
                u8_dir,
                u16le_dir,
                u24le_dir,
                AsmInvokeAst::parser().map(AsmStmtAst::Invoke),
            ))
        })
    }
}

//===========================================================================//

/// The abstract syntax tree for a section declaration in an assembly file.
#[derive(Clone, Debug)]
pub struct AsmSectionAst {
    /// A static expression that evaluates to the name of the section that this
    /// chunk belongs to.
    pub name: ExprAst,
    /// The statements inside the section block.
    pub body: Vec<AsmStmtAst>,
}

//===========================================================================//

/// Represents a line in an assembly file that invokes a macro.
#[derive(Clone, Debug)]
pub struct AsmInvokeAst {
    /// The name of the macro to invoke.
    pub id: IdentifierAst,
    /// The arguments to the macro, if any.
    pub args: Vec<Vec<Token>>,
}

impl AsmInvokeAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmInvokeAst, PError<'a>> + Clone {
        // TODO: Require delimiters to be balanced, and allow commas within
        // delimiters.
        let macro_arg = chumsky::prelude::any()
            .filter(|token: &Token| {
                !matches!(
                    token.value,
                    TokenValue::Comma | TokenValue::Linebreak
                )
            })
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>();
        let macro_args = macro_arg
            .separated_by(symbol(TokenValue::Comma))
            .collect::<Vec<_>>();
        IdentifierAst::parser()
            .then(macro_args)
            .then_ignore(linebreak())
            .map(|(id, args)| AsmInvokeAst { id, args })
    }
}

//===========================================================================//
