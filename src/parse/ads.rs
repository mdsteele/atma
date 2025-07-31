//! Facilities for parsing Atma Debugger Script.

use super::expr::{ExprAst, IdentifierAst, PError, symbol};
use crate::parse::{ParseError, Token, TokenValue};
use chumsky::{self, IterParser, Parser};

//===========================================================================//

/// The abstract syntax tree for an entire Atma Debugger Script module.
pub struct AdsModuleAst {
    statements: Vec<AdsStmtAst>,
}

impl AdsModuleAst {
    /// Parses a sequence of tokens into an Atma Debugger Script module.
    pub fn parse(tokens: &[Token]) -> Result<AdsModuleAst, Vec<ParseError>> {
        AdsModuleAst::parser().parse(tokens).into_result().map_err(|errors| {
            errors
                .into_iter()
                .map(|error| {
                    let index = error.span().start;
                    let location = if index < tokens.len() {
                        tokens[index].start
                    } else {
                        tokens[tokens.len() - 1].start
                    };
                    let message = format!("{error:?}");
                    ParseError { location, message }
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

    /// Returns the list of top-level statements in this module.
    pub fn statements(&self) -> &[AdsStmtAst] {
        &self.statements
    }
}

//===========================================================================//

/// The abstract syntax tree for one statement in an Atma Debugger Script
/// module.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AdsStmtAst {
    /// Exits the script.
    Exit,
    /// Defines a constant.
    Let(IdentifierAst, ExprAst),
    /// A no-op statement.
    Relax,
}

impl AdsStmtAst {
    fn parser<'a>()
    -> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
        chumsky::prelude::choice((
            exit_statement(),
            let_statement(),
            relax_statement(),
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

fn exit_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("exit").then_ignore(linebreak()).to(AdsStmtAst::Exit)
}

fn let_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("let")
        .ignore_then(IdentifierAst::parser())
        .then_ignore(symbol(TokenValue::Equals))
        .then(ExprAst::parser())
        .then_ignore(linebreak())
        .map(|(id, expr)| AdsStmtAst::Let(id, expr))
}

fn relax_statement<'a>()
-> impl Parser<'a, &'a [Token], AdsStmtAst, PError<'a>> + Clone {
    keyword("relax").then_ignore(linebreak()).to(AdsStmtAst::Relax)
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsModuleAst, AdsStmtAst, ExprAst, IdentifierAst};
    use crate::parse::{ParseError, SrcLoc, Token, TokenLexer};
    use num_bigint::BigInt;

    fn read_statements(input: &str) -> Vec<AdsStmtAst> {
        AdsModuleAst::parse(
            &TokenLexer::new(input.as_bytes())
                .collect::<Result<Vec<Token>, ParseError>>()
                .unwrap(),
        )
        .unwrap()
        .statements()
        .to_vec()
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
            vec![AdsStmtAst::Let(
                IdentifierAst {
                    id: "foo".to_string(),
                    start: SrcLoc { line: 1, column: 4 }
                },
                ExprAst::IntLiteral(BigInt::from(42))
            )]
        );
    }

    #[test]
    fn relax_statement() {
        assert_eq!(read_statements("relax\n"), vec![AdsStmtAst::Relax]);
    }
}

//===========================================================================//
