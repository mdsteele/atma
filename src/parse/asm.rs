//! Facilities for parsing assembly source code.

use super::atom::{PError, directive, linebreak, symbol};
use super::expr::{ExprAst, IdentifierAst};
use super::lex::{Token, TokenLexer, TokenValue};
use super::types::ParseError;
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
    pub fn parse_source(
        source: &str,
    ) -> Result<AsmModuleAst, Vec<ParseError>> {
        let lexer = TokenLexer::new(source);
        let tokens: Vec<Token> =
            lexer.collect::<Result<_, _>>().map_err(|error| vec![error])?;
        AsmStmtAst::parser()
            .repeated()
            .collect::<Vec<_>>()
            .map(|statements| AsmModuleAst { statements })
            .parse(&tokens)
            .into_result()
            .map_err(|errors| {
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
}

//===========================================================================//

/// The abstract syntax tree for a single statement or declaration in an
/// assembly file.
#[derive(Debug)]
pub enum AsmStmtAst {
    /// A macro invocation.
    Invoke(AsmMacroLine),
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
                section_dir,
                u8_dir,
                u16le_dir,
                u24le_dir,
                AsmMacroLine::parser().map(AsmStmtAst::Invoke),
            ))
        })
    }
}

//===========================================================================//

/// The abstract syntax tree for a section declaration in an assembly file.
#[derive(Debug)]
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
pub struct AsmMacroLine {
    /// The name of the macro to invoke.
    pub name: IdentifierAst,
    /// The arguments to the macro, if any.
    pub args: Vec<Token>,
}

impl AsmMacroLine {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmMacroLine, PError<'a>> + Clone {
        IdentifierAst::parser()
            .then(
                chumsky::prelude::any()
                    .filter(|token: &Token| {
                        !matches!(token.value, TokenValue::Linebreak)
                    })
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(linebreak())
            .map(|(name, args)| AsmMacroLine { name, args })
    }
}

//===========================================================================//
