//! Facilities for parsing Atma Debugger Script.

use crate::parse::{
    LexResult, SrcLoc, StreamResult, Token, TokenLexer, TokenValue,
};

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

/// An error encountered while parsing a source code file.
#[derive(Debug, Eq, PartialEq)]
pub struct ParseError {
    /// The location in the file where the error occurred.
    pub location: SrcLoc,
    /// The error message to report to the user.
    pub message: String,
    /// False if parsing can continue past the error, or true if parsing must
    /// terminate.
    pub is_fatal: bool,
}

/// A partial result from a stream of parsed statements.
pub type ParseResult = StreamResult<AdsStatement, ParseError>;

//===========================================================================//

enum ParseState {
    Nothing,
    Error,
    CompleteStatement(AdsStatement),
}

/// A parser for extracting a sequence of statements from an Atma Debugger
/// Script source file.
pub struct AdsLineParser {
    lexer: TokenLexer,
    state: ParseState,
}

impl AdsLineParser {
    /// Constructs a ADS line parser in its initial state.
    #[allow(clippy::new_without_default)]
    pub fn new() -> AdsLineParser {
        AdsLineParser { lexer: TokenLexer::new(), state: ParseState::Nothing }
    }

    /// Given the next chunk of input bytes, returns the number of bytes
    /// consumed and the next statement in the stream (if any).  Pass an empty
    /// buffer to indicate the end of input.
    pub fn read_from(&mut self, mut buffer: &[u8]) -> (usize, ParseResult) {
        let mut total_consumed: usize = 0;
        loop {
            let (consumed, lexer_result) = self.lexer.read_from(buffer);
            total_consumed += consumed;
            match lexer_result {
                LexResult::NeedMoreInput => {
                    return (total_consumed, ParseResult::NeedMoreInput);
                }
                LexResult::Yield(token) => {
                    let result = if let TokenValue::Linebreak = token.value {
                        self.on_end_of_line()
                    } else {
                        self.on_token(token)
                    };
                    if let ParseResult::NeedMoreInput = result {
                        buffer = &buffer[consumed..];
                        if !buffer.is_empty() {
                            continue;
                        }
                    }
                    return (total_consumed, result);
                }
                LexResult::NoMoreOutput => {
                    return (total_consumed, self.on_no_more_tokens());
                }
                LexResult::Error(lex_error) => {
                    self.state = ParseState::Error;
                    return (
                        total_consumed,
                        ParseResult::Error(ParseError {
                            location: lex_error.location,
                            message: format!(
                                "lexical error: {}",
                                lex_error.message
                            ),
                            is_fatal: true,
                        }),
                    );
                }
            };
        }
    }

    fn on_token(&mut self, token: Token) -> ParseResult {
        match &mut self.state {
            // If the line parser is in an error state, ignore all subsequent
            // tokens until a linebreak is reached.
            ParseState::Error => ParseResult::NeedMoreInput,
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
                        ParseResult::NeedMoreInput
                    }
                    "relax" => {
                        self.state =
                            ParseState::CompleteStatement(AdsStatement::Relax);
                        ParseResult::NeedMoreInput
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

    fn on_end_of_line(&mut self) -> ParseResult {
        match std::mem::replace(&mut self.state, ParseState::Nothing) {
            // If the line parser is in an error state, quietly put it back
            // into the `Nothing` state once the linebreak is reached so it can
            // try to parse the next line.
            ParseState::Error => ParseResult::NeedMoreInput,
            ParseState::Nothing => ParseResult::NeedMoreInput,
            ParseState::CompleteStatement(stmt) => ParseResult::Yield(stmt),
        }
    }

    fn on_no_more_tokens(&mut self) -> ParseResult {
        if let ParseState::Nothing = self.state {
            ParseResult::NoMoreOutput
        } else {
            self.on_end_of_line()
        }
    }

    fn token_error(&mut self, token: Token, message: String) -> ParseResult {
        self.state = ParseState::Error;
        let error =
            ParseError { location: token.start, message, is_fatal: false };
        ParseResult::Error(error)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsLineParser, AdsStatement, ParseResult};

    fn read_all(mut input: &[u8]) -> Vec<AdsStatement> {
        let mut parser = AdsLineParser::new();
        let mut statements = Vec::<AdsStatement>::new();
        loop {
            let (consumed, result) = parser.read_from(input);
            match result {
                ParseResult::NeedMoreInput => {
                    if input.is_empty() {
                        panic!("NeedMoreInput");
                    }
                }
                ParseResult::Yield(stmt) => statements.push(stmt),
                ParseResult::NoMoreOutput => break,
                ParseResult::Error(error) => panic!("Error({error:?})"),
            }
            input = &input[consumed..];
        }
        statements
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
