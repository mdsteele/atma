//! Facilities for parsing Atma Debugger Script.

use super::expr::{ExprAst, IdentifierAst, PError, symbol};
use super::lvalue::LValueAst;
use crate::parse::{ParseError, Token, TokenLexer, TokenValue};
use chumsky::{self, IterParser, Parser};

//===========================================================================//

/// The abstract syntax tree for an entire Atma Debugger Script module.
pub struct AdsModuleAst {
    /// The statements in this module.
    pub statements: Vec<AdsStmtAst>,
}

impl AdsModuleAst {
    /// Reads the abstract syntax tree for an Atma Debugger Script module from
    /// a file.
    pub fn parse_source(
        source: &str,
    ) -> Result<AdsModuleAst, Vec<ParseError>> {
        let lexer = TokenLexer::new(source);
        let tokens: Vec<Token> =
            lexer.collect::<Result<_, _>>().map_err(|error| vec![error])?;
        AdsModuleAst::parse(&tokens)
    }

    /// Parses a sequence of tokens into an Atma Debugger Script module.
    pub fn parse(tokens: &[Token]) -> Result<AdsModuleAst, Vec<ParseError>> {
        AdsModuleAst::parser().parse(tokens).into_result().map_err(|errors| {
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
    -> impl Parser<'a, &'a [Token], AdsModuleAst, PError<'a>> + Clone {
        symbol(TokenValue::Linebreak).repeated().ignore_then(
            AdsStmtAst::parser()
                .repeated()
                .collect::<Vec<_>>()
                .map(|statements| AdsModuleAst { statements }),
        )
    }
}

//===========================================================================//

/// The abstract syntax tree for one statement in an Atma Debugger Script
/// module.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AdsStmtAst {
    /// Declares a constant or variable.
    Declare(DeclareAst, IdentifierAst, ExprAst),
    /// Exits the script.
    Exit,
    /// Executes the first block if the expression is true, the second block
    /// otherwise.
    If(ExprAst, Vec<AdsStmtAst>, Vec<AdsStmtAst>),
    /// Prints the value of an expression.
    Print(ExprAst),
    /// A no-op statement.
    Relax,
    /// Runs the simulated processor until the specified breakpoint is reached.
    RunUntil(BreakpointAst),
    /// Updates a variable.
    Set(LValueAst, ExprAst),
    /// Steps the simulated processor forward by one instruction.
    Step,
    /// Sets up a breakpoint handler.
    When(BreakpointAst, Vec<AdsStmtAst>),
}

impl AdsStmtAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
        chumsky::prelude::recursive(|statement| {
            let stmt_block = symbol(TokenValue::BraceOpen)
                .ignore_then(linebreak())
                .ignore_then(statement.repeated().collect::<Vec<_>>())
                .then_ignore(symbol(TokenValue::BraceClose));
            let if_statement = keyword("if")
                .ignore_then(ExprAst::parser())
                .then(stmt_block.clone())
                .then(keyword("else").ignore_then(stmt_block.clone()).or_not())
                .then_ignore(linebreak())
                .map(|((p, t), e)| {
                    AdsStmtAst::If(p, t, e.unwrap_or_else(Vec::new))
                });
            let when_statement = keyword("when")
                .ignore_then(BreakpointAst::parser())
                .then(stmt_block)
                .then_ignore(linebreak())
                .map(|(cond, block)| AdsStmtAst::When(cond, block));
            chumsky::prelude::choice((
                declare_statement(),
                exit_statement(),
                if_statement,
                print_statement(),
                relax_statement(),
                run_until_statement(),
                set_statement(),
                step_statement(),
                when_statement,
            ))
        })
    }
}

//===========================================================================//

/// The kind of variable declaration.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeclareAst {
    /// Declares a new constant.
    Let,
    /// Declares a new variable.
    Var,
}

impl DeclareAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], DeclareAst, PError<'a>> + Clone {
        chumsky::prelude::choice((
            keyword("let").to(DeclareAst::Let),
            keyword("var").to(DeclareAst::Var),
        ))
    }
}

//===========================================================================//

/// The abstract syntax tree for a breakpoint condition in an Atma Debugger
/// Script program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BreakpointAst {
    /// Triggers when the PC is at the specified label or address.
    Pc(ExprAst),
    /// Triggers when the processor tries to read a byte from the specified
    /// label or address.
    Read(ExprAst),
    /// Triggers when the processor tries to write a byte to the specified
    /// label or address.
    Write(ExprAst),
}

impl BreakpointAst {
    pub(crate) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], BreakpointAst, PError<'a>> + Clone {
        chumsky::prelude::choice((
            keyword("at")
                .ignore_then(ExprAst::parser())
                .map(BreakpointAst::Pc),
            keyword("read")
                .ignore_then(ExprAst::parser())
                .map(BreakpointAst::Read),
            keyword("write")
                .ignore_then(ExprAst::parser())
                .map(BreakpointAst::Write),
        ))
    }
}

//===========================================================================//

fn keyword<'a>(
    word: &'static str,
) -> impl Parser<'a, &'a [Token], (), PError<'a>> + Clone {
    chumsky::prelude::any()
        .filter(move |token: &Token| {
            if let TokenValue::Identifier(id) = &token.value {
                id == word
            } else {
                false
            }
        })
        .ignored()
        .labelled(word)
}

fn linebreak<'a>() -> impl Parser<'a, &'a [Token], (), PError<'a>> + Clone {
    symbol(TokenValue::Linebreak).repeated().at_least(1)
}

fn declare_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    DeclareAst::parser()
        .then(IdentifierAst::parser())
        .then_ignore(symbol(TokenValue::Equals))
        .then(ExprAst::parser())
        .then_ignore(linebreak())
        .map(|((declare, id), expr)| AdsStmtAst::Declare(declare, id, expr))
}

fn exit_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("exit").ignore_then(linebreak()).to(AdsStmtAst::Exit)
}

fn print_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("print")
        .ignore_then(ExprAst::parser())
        .then_ignore(linebreak())
        .map(AdsStmtAst::Print)
}

fn relax_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("relax").ignore_then(linebreak()).to(AdsStmtAst::Relax)
}

fn run_until_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("run")
        .ignore_then(keyword("until"))
        .ignore_then(BreakpointAst::parser())
        .then_ignore(linebreak())
        .map(AdsStmtAst::RunUntil)
}

fn set_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("set")
        .ignore_then(LValueAst::parser())
        .then_ignore(symbol(TokenValue::Equals))
        .then(ExprAst::parser())
        .then_ignore(linebreak())
        .map(|(id, expr)| AdsStmtAst::Set(id, expr))
}

fn step_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("step").ignore_then(linebreak()).to(AdsStmtAst::Step)
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsModuleAst, AdsStmtAst, DeclareAst, IdentifierAst};
    use crate::parse::{
        ExprAst, ExprAstNode, ParseError, SrcSpan, Token, TokenLexer,
    };
    use num_bigint::BigInt;

    fn read_statements(input: &str) -> Vec<AdsStmtAst> {
        AdsModuleAst::parse(
            &TokenLexer::new(input)
                .collect::<Result<Vec<Token>, ParseError>>()
                .unwrap(),
        )
        .unwrap()
        .statements
    }

    #[test]
    fn empty_input() {
        assert_eq!(read_statements(""), vec![]);
    }

    #[test]
    fn extra_linebreaks() {
        assert_eq!(
            read_statements(
                "relax  ; just rest\n\
                  \n\n\
                  exit  ; OK, all done!\n"
            ),
            vec![AdsStmtAst::Relax, AdsStmtAst::Exit]
        );
    }

    #[test]
    fn leading_linebreak() {
        assert_eq!(read_statements("\nrelax\n"), vec![AdsStmtAst::Relax]);
    }

    #[test]
    fn exit_statement() {
        assert_eq!(read_statements("exit\n"), vec![AdsStmtAst::Exit]);
    }

    #[test]
    fn let_statement() {
        assert_eq!(
            read_statements("let foo = 42\n"),
            vec![AdsStmtAst::Declare(
                DeclareAst::Let,
                IdentifierAst {
                    span: SrcSpan::from_byte_range(4..7),
                    name: "foo".to_string(),
                },
                ExprAst {
                    span: SrcSpan::from_byte_range(10..12),
                    node: ExprAstNode::IntLiteral(BigInt::from(42)),
                },
            )]
        );
    }

    #[test]
    fn print_statement() {
        assert_eq!(
            read_statements("print 42\n"),
            vec![AdsStmtAst::Print(ExprAst {
                span: SrcSpan::from_byte_range(6..8),
                node: ExprAstNode::IntLiteral(BigInt::from(42))
            })]
        );
    }

    #[test]
    fn relax_statement() {
        assert_eq!(read_statements("relax\n"), vec![AdsStmtAst::Relax]);
    }
}

//===========================================================================//
