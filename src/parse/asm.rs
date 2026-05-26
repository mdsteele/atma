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
    /// An anonymous local scope.
    AnonymousScope(Vec<AsmStmtAst>),
    /// An `.ASSERT` directive.
    Assert(AsmAssertAst),
    /// A `.DEFMACRO` directive.
    DefMacro(AsmDefMacroAst),
    /// An `.IMPORT` directive.
    Import(IdentifierAst),
    /// An integer data directive (e.g. `.U8` or `.U16LE`).
    IntData(AsmIntDataAst),
    /// A macro invocation.
    Invoke(AsmInvokeAst),
    /// A label.
    Label(IdentifierAst),
    /// A named local scope.
    NamedScope(IdentifierAst, Vec<AsmStmtAst>),
    /// A `.RESERVE` directive.
    Reserve(AsmReserveAst),
    /// A `.SECTION` block.
    Section(AsmSectionAst),
    /// A `.UTF8` directive.
    Utf8Data(AsmUtf8DataAst),
}

impl AsmStmtAst {
    fn parser<'a>() -> impl Parser<'a, &'a [Token], AsmStmtAst, Extra<'a>> {
        let attributes = symbol(TokenValue::Comma)
            .ignore_then(IdentifierAst::parser())
            .then_ignore(symbol(TokenValue::Equals))
            .then(ExprAst::parser())
            .repeated()
            .collect::<Vec<_>>();
        chumsky::prelude::recursive(|statement| {
            let stmt_block = symbol(TokenValue::BraceOpen)
                .ignore_then(linebreak())
                .ignore_then(statement.repeated().collect::<Vec<_>>())
                .then_ignore(symbol(TokenValue::BraceClose));
            let anonymous_scope = stmt_block
                .clone()
                .then_ignore(linebreak())
                .map(AsmStmtAst::AnonymousScope);
            let label_or_named_scope = IdentifierAst::parser()
                .then_ignore(symbol(TokenValue::Colon))
                .then(stmt_block.clone().then_ignore(linebreak()).or_not())
                .then_ignore(symbol(TokenValue::Linebreak).repeated())
                .map(|(id, opt_scope)| {
                    if let Some(body) = opt_scope {
                        AsmStmtAst::NamedScope(id, body)
                    } else {
                        AsmStmtAst::Label(id)
                    }
                });
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
                .then(attributes)
                .then(stmt_block)
                .then_ignore(linebreak())
                .map(|((name, attrs), body)| {
                    AsmStmtAst::Section(AsmSectionAst { name, attrs, body })
                });
            chumsky::prelude::choice((
                anonymous_scope,
                label_or_named_scope,
                AsmAssertAst::parser().map(AsmStmtAst::Assert),
                def_macro_dir,
                import_dir,
                AsmReserveAst::parser().map(AsmStmtAst::Reserve),
                section_dir,
                AsmIntDataAst::parser().map(AsmStmtAst::IntData),
                AsmInvokeAst::parser().map(AsmStmtAst::Invoke),
                AsmUtf8DataAst::parser().map(AsmStmtAst::Utf8Data),
            ))
        })
    }
}

//===========================================================================//

/// The abstract syntax tree for an assertion in an assembly file.
#[derive(Clone, Debug)]
pub struct AsmAssertAst {
    /// The boolean condition that is expected to be true.
    pub condition: ExprAst,
    /// An optional error message that should be emitted if the assertion
    /// fails.
    pub message: Option<ExprAst>,
}

impl AsmAssertAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmAssertAst, Extra<'a>> + Clone {
        directive(".ASSERT")
            .ignore_then(ExprAst::parser())
            .then(
                symbol(TokenValue::Comma)
                    .ignore_then(ExprAst::parser())
                    .or_not(),
            )
            .then_ignore(linebreak())
            .map(|(condition, message)| AsmAssertAst { condition, message })
    }
}

//===========================================================================//

/// The abstract syntax tree for a reference to a data type in an assembly
/// file.
#[derive(Clone, Debug)]
pub enum AsmDataTypeAst {
    /// An integer data type.
    Int(SrcSpan, AsmIntTypeAst),
    // TODO: Struct(IdentifierAst),
}

impl AsmDataTypeAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmDataTypeAst, Extra<'a>> + Clone {
        AsmIntTypeAst::parser()
            .map(|(span, int_type)| AsmDataTypeAst::Int(span, int_type))
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

/// The abstract syntax tree for an integer data directive in an assembly file.
#[derive(Clone, Debug)]
pub struct AsmIntDataAst {
    /// The location in the source code where the directive token appears.
    pub directive_span: SrcSpan,
    /// The type of integer data.
    pub int_type: AsmIntTypeAst,
    /// The expressions for the integer data to insert.
    pub expressions: Vec<ExprAst>,
}

impl AsmIntDataAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmIntDataAst, Extra<'a>> + Clone {
        AsmIntTypeAst::parser()
            .then(
                ExprAst::parser()
                    .separated_by(symbol(TokenValue::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(linebreak())
            .map(|((directive_span, int_type), expressions)| AsmIntDataAst {
                directive_span,
                int_type,
                expressions,
            })
    }
}

//===========================================================================//

/// Types of integer data directives in assembly code.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AsmIntTypeAst {
    /// An 8-bit unsigned integer.
    U8,
    /// A 16-bit unsigned integer, using the current architecture's native
    /// endianness.
    U16,
    /// A 16-bit unsigned big-endian integer.
    U16be,
    /// A 16-bit unsigned little-endian integer.
    U16le,
    /// A 24-bit unsigned integer, using the current architecture's native
    /// endianness.
    U24,
    /// A 24-bit unsigned big-endian integer.
    U24be,
    /// A 24-bit unsigned little-endian integer.
    U24le,
}

impl AsmIntTypeAst {
    const ALL: &[AsmIntTypeAst] = &[
        AsmIntTypeAst::U8,
        AsmIntTypeAst::U16,
        AsmIntTypeAst::U16be,
        AsmIntTypeAst::U16le,
        AsmIntTypeAst::U24,
        AsmIntTypeAst::U24be,
        AsmIntTypeAst::U24le,
    ];

    pub(crate) fn directive(self) -> &'static str {
        match self {
            AsmIntTypeAst::U8 => ".U8",
            AsmIntTypeAst::U16 => ".U16",
            AsmIntTypeAst::U16be => ".U16BE",
            AsmIntTypeAst::U16le => ".U16LE",
            AsmIntTypeAst::U24 => ".U24",
            AsmIntTypeAst::U24be => ".U24BE",
            AsmIntTypeAst::U24le => ".U24LE",
        }
    }

    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], (SrcSpan, AsmIntTypeAst), Extra<'a>> + Clone
    {
        chumsky::prelude::choice(
            AsmIntTypeAst::ALL
                .iter()
                .copied()
                .map(|int_type| {
                    directive(int_type.directive())
                        .map(move |span| (span, int_type))
                })
                .collect::<Vec<_>>(),
        )
    }
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

/// The abstract syntax tree for a reserve declaration in an assembly file.
#[derive(Clone, Debug)]
pub struct AsmReserveAst {
    /// The location in the source code where the directive token appears.
    pub directive_span: SrcSpan,
    /// The type of data to reserve space for.
    pub data_type: AsmDataTypeAst,
    /// Optionally, how many instances of the data type to reserve space for
    /// (rather than the default of one).
    pub count: Option<ExprAst>,
}

impl AsmReserveAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmReserveAst, Extra<'a>> + Clone {
        directive(".RESERVE")
            .then(
                AsmDataTypeAst::parser().then(
                    symbol(TokenValue::Comma)
                        .ignore_then(ExprAst::parser())
                        .or_not(),
                ),
            )
            .then_ignore(linebreak())
            .map(|(directive_span, (data_type, count))| AsmReserveAst {
                directive_span,
                data_type,
                count,
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
    /// Key/value attributes associated with this chunk.
    pub attrs: Vec<(IdentifierAst, ExprAst)>,
    /// The statements inside the section block.
    pub body: Vec<AsmStmtAst>,
}

//===========================================================================//

/// The abstract syntax tree for a UTF8 data directive in an assembly file.
#[derive(Clone, Debug)]
pub struct AsmUtf8DataAst {
    /// The location in the source code where the directive token appears.
    pub directive_span: SrcSpan,
    /// The expressions for the string data to insert.
    pub expressions: Vec<ExprAst>,
}

impl AsmUtf8DataAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AsmUtf8DataAst, Extra<'a>> + Clone {
        directive(".UTF8")
            .then(
                ExprAst::parser()
                    .separated_by(symbol(TokenValue::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(linebreak())
            .map(|(directive_span, expressions)| AsmUtf8DataAst {
                directive_span,
                expressions,
            })
    }
}

//===========================================================================//
