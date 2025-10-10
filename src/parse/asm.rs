//! Facilities for parsing assembly source code.

use super::atom::{PError, directive, linebreak};
use super::expr::{ExprAst, IdentifierAst};
use super::lex::{Token, TokenLexer, TokenValue};
use super::types::ParseError;
use chumsky::{self, IterParser, Parser};

//===========================================================================//

/// The abstract syntax tree for one line in an assembly file.
#[derive(Clone, Debug)]
pub enum AsmRawLine {
    /// A macro invocation.
    Macro(AsmMacroLine),
    /// A non-macro line.
    Final(AsmFinalLine),
}

impl AsmRawLine {
    /// Parses assembly source code into a sequence of lines.
    pub fn parse(source: &str) -> Result<Vec<AsmRawLine>, Vec<ParseError>> {
        let lexer = TokenLexer::new(source);
        let tokens: Vec<Token> =
            lexer.collect::<Result<_, _>>().map_err(|error| vec![error])?;
        AsmRawLine::parser()
            .repeated()
            .collect::<Vec<_>>()
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

    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmRawLine, PError<'a>> + Clone {
        chumsky::prelude::choice((
            AsmMacroLine::parser().map(AsmRawLine::Macro),
            AsmFinalLine::parser().map(AsmRawLine::Final),
        ))
    }
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

/// Represents a line in an assembly file that has no more macros to expand.
#[derive(Clone, Debug)]
pub enum AsmFinalLine {
    /// A `.SECTION` directive (starting a new section chunk).
    Section(ExprAst),
    /// A `.U8` directive (declaring unsigned byte data).
    U8(ExprAst),
}

impl AsmFinalLine {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmFinalLine, PError<'a>> + Clone {
        let section_dir = directive(".SECTION")
            .ignore_then(ExprAst::parser())
            .map(AsmFinalLine::Section);
        let u8_dir = directive(".U8")
            .ignore_then(ExprAst::parser())
            .map(AsmFinalLine::U8);
        chumsky::prelude::choice((section_dir, u8_dir))
            .then_ignore(linebreak())
    }
}

//===========================================================================//
