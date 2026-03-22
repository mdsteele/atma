//! Facilities for parsing assembly source code.

use super::atom::{
    Context, Extra, directive, linebreak, parse_tokens, symbol, tokenize,
};
use super::error::ParseResult;
use super::expr::{ExprAst, IdentifierAst};
use super::lex::{Token, TokenValue};
use crate::error::SrcSpan;
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
    /// A `.DEFMACRO` directive.
    DefMacro(AsmDefMacroAst),
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
    fn parser<'a>() -> impl Parser<'a, &'a [Token], AsmStmtAst, Extra<'a>> {
        chumsky::prelude::recursive(|statement| {
            let label = IdentifierAst::parser()
                .then_ignore(symbol(TokenValue::Colon))
                .then_ignore(symbol(TokenValue::Linebreak).repeated())
                .map(AsmStmtAst::Label);
            let stmt_block = symbol(TokenValue::BraceOpen)
                .ignore_then(linebreak())
                .ignore_then(statement.repeated().collect::<Vec<_>>())
                .then_ignore(symbol(TokenValue::BraceClose));
            let def_macro_dir = directive(".DEFMACRO")
                .ignore_then(IdentifierAst::parser())
                .then(
                    AsmMacroArgAst::parser()
                        .separated_by(symbol(TokenValue::Comma))
                        .collect::<Vec<_>>(),
                )
                .then(stmt_block.clone().with_ctx(Context {
                    allow_placeholder_as_identifier: true,
                }))
                .then_ignore(linebreak())
                .map(|((id, params), body)| {
                    AsmStmtAst::DefMacro(AsmDefMacroAst { id, params, body })
                });
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
                def_macro_dir,
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

/// The abstract syntax tree for a macro definition in an assembly file.
#[derive(Clone, Debug)]
pub struct AsmDefMacroAst {
    /// The name of the macro.
    pub id: IdentifierAst,
    /// The parameters for the macro, if any.
    pub params: Vec<AsmMacroArgAst>,
    /// The statements inside the macro body.
    pub body: Vec<AsmStmtAst>,
}

//===========================================================================//

/// Represents a line in an assembly file that invokes a macro.
#[derive(Clone, Debug)]
pub struct AsmInvokeAst {
    /// The name of the macro to invoke.
    pub id: IdentifierAst,
    /// The arguments to the macro, if any.
    pub args: Vec<AsmMacroArgAst>,
}

impl AsmInvokeAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmInvokeAst, Extra<'a>> + Clone {
        let macro_args = AsmMacroArgAst::parser()
            .separated_by(symbol(TokenValue::Comma))
            .collect::<Vec<_>>();
        IdentifierAst::parser()
            .then(macro_args)
            .then_ignore(linebreak())
            .map(|(id, args)| AsmInvokeAst { id, args })
    }
}

//===========================================================================//

/// One argument for a macro.
#[derive(Clone, Debug)]
pub struct AsmMacroArgAst {
    /// The location in the source code where this macro argument appears.
    pub span: SrcSpan,
    /// The tokens for this argument.
    pub tokens: Vec<Token>,
}

impl AsmMacroArgAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmMacroArgAst, Extra<'a>> + Clone {
        let simple = chumsky::prelude::any().filter(|token: &Token| {
            !matches!(
                token.value,
                TokenValue::BraceClose
                    | TokenValue::BraceOpen
                    | TokenValue::BracketClose
                    | TokenValue::BracketOpen
                    | TokenValue::Comma
                    | TokenValue::Directive(_)
                    | TokenValue::Linebreak
                    | TokenValue::ParenClose
                    | TokenValue::ParenOpen
            )
        });
        let compound = chumsky::prelude::recursive(|compound| {
            let inner = symbol(TokenValue::Comma)
                .or(simple)
                .repeated()
                .at_least(1)
                .collect::<Vec<Token>>()
                .or(compound)
                .repeated()
                .collect::<Vec<Vec<Token>>>()
                .map(|token_vecs: Vec<Vec<Token>>| token_vecs.concat());
            let group = |open_symbol: TokenValue, close_symbol: TokenValue| {
                chumsky::prelude::group((
                    symbol(open_symbol),
                    inner.clone(),
                    symbol(close_symbol),
                ))
                .map(
                    |(open, mut tokens, close): (Token, Vec<Token>, Token)| {
                        tokens.insert(0, open);
                        tokens.push(close);
                        tokens
                    },
                )
            };
            chumsky::prelude::choice((
                group(TokenValue::BraceOpen, TokenValue::BraceClose),
                group(TokenValue::BracketOpen, TokenValue::BracketClose),
                group(TokenValue::ParenOpen, TokenValue::ParenClose),
            ))
        });
        simple
            .repeated()
            .at_least(1)
            .collect::<Vec<Token>>()
            .or(compound)
            .repeated()
            .at_least(1)
            .collect::<Vec<Vec<Token>>>()
            .map(|token_vecs| {
                let tokens = token_vecs.concat();
                let span = tokens
                    .first()
                    .unwrap()
                    .span
                    .merged_with(tokens.last().unwrap().span);
                AsmMacroArgAst { span, tokens }
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
