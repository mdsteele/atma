//! Facilities for parsing Atma Debugger Script.

use crate::parse::{ParseError, Token, TokenLexer, TokenValue};

//===========================================================================//

/// One statement in an Atma Debugger Script source file.
#[derive(Debug, Eq, PartialEq)]
pub enum AdsStatement {
    /// Exits the script.
    Exit,
    /// A no-op statement.
    Relax,
}

impl AdsStatement {
    /// Returns the human-readable name for this kind of statement.
    pub fn name(&self) -> &str {
        match &self {
            AdsStatement::Exit => "exit",
            AdsStatement::Relax => "relax",
        }
    }
}

//===========================================================================//

enum ParseState {
    Nothing,
    Error,
    CompleteStatement(AdsStatement),
}

/// A parser for extracting a sequence of statements from an Atma Debugger
/// Script source file.
pub struct AdsLineParser<'a> {
    lexer: TokenLexer<'a>,
    state: ParseState,
}

impl<'a> AdsLineParser<'a> {
    /// Constructs a ADS line parser in its initial state.
    pub fn new(input: &[u8]) -> AdsLineParser {
        AdsLineParser {
            lexer: TokenLexer::new(input),
            state: ParseState::Nothing,
        }
    }

    fn on_token(
        &mut self,
        token: Token,
    ) -> Option<Result<AdsStatement, ParseError>> {
        match &mut self.state {
            // If the line parser is in an error state, ignore all subsequent
            // tokens until a linebreak is reached.
            ParseState::Error => None,
            ParseState::CompleteStatement(stmt) => {
                let message = format!(
                    "unexpected {} after {} statement",
                    token.value.name(),
                    stmt.name()
                );
                self.token_error(token, message)
            }
            ParseState::Nothing => match &token.value {
                TokenValue::Identifier(id) => match id.as_str() {
                    "exit" => {
                        self.state =
                            ParseState::CompleteStatement(AdsStatement::Exit);
                        None
                    }
                    "relax" => {
                        self.state =
                            ParseState::CompleteStatement(AdsStatement::Relax);
                        None
                    }
                    _ => {
                        let message = format!("invalid command: {id}");
                        self.token_error(token, message)
                    }
                },
                TokenValue::Linebreak => unreachable!(),
                value => {
                    let message = format!(
                        "cannot start a statement with {}",
                        value.name()
                    );
                    self.token_error(token, message)
                }
            },
        }
    }

    fn on_end_of_line(&mut self) -> Option<Result<AdsStatement, ParseError>> {
        match std::mem::replace(&mut self.state, ParseState::Nothing) {
            // If the line parser is in an error state, quietly put it back
            // into the `Nothing` state once the linebreak is reached so it can
            // try to parse the next line.
            ParseState::Error => None,
            ParseState::Nothing => None,
            ParseState::CompleteStatement(stmt) => Some(Ok(stmt)),
        }
    }

    fn on_no_more_tokens(
        &mut self,
    ) -> Option<Result<AdsStatement, ParseError>> {
        self.on_end_of_line()
    }

    fn token_error(
        &mut self,
        token: Token,
        message: String,
    ) -> Option<Result<AdsStatement, ParseError>> {
        self.state = ParseState::Error;
        Some(Err(ParseError { location: token.start, message }))
    }
}

impl<'a> Iterator for AdsLineParser<'a> {
    type Item = Result<AdsStatement, ParseError>;

    fn next(&mut self) -> Option<Result<AdsStatement, ParseError>> {
        loop {
            match self.lexer.next() {
                Some(Ok(token)) => {
                    let option = if let TokenValue::Linebreak = token.value {
                        self.on_end_of_line()
                    } else {
                        self.on_token(token)
                    };
                    if let Some(result) = option {
                        return Some(result);
                    }
                }
                Some(Err(error)) => {
                    self.state = ParseState::Error;
                    return Some(Err(error));
                }
                None => return self.on_no_more_tokens(),
            };
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsLineParser, AdsStatement};

    fn read_all(input: &[u8]) -> Vec<AdsStatement> {
        AdsLineParser::new(input).collect::<Result<_, _>>().unwrap()
    }

    #[test]
    fn empty_input() {
        assert_eq!(read_all(b""), vec![]);
    }

    #[test]
    fn extra_linebreaks() {
        assert_eq!(
            read_all(
                b"relax  ; just rest\n\
                  \n\n\
                  exit  ; OK, all done!\n"
            ),
            vec![AdsStatement::Relax, AdsStatement::Exit]
        );
    }

    #[test]
    fn exit_statement() {
        assert_eq!(read_all(b"exit\n"), vec![AdsStatement::Exit]);
    }

    #[test]
    fn relax_statement() {
        assert_eq!(read_all(b"relax\n"), vec![AdsStatement::Relax]);
    }
}

//===========================================================================//
