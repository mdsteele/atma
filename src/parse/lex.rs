use crate::parse::{SrcLoc, StreamResult};
use num_bigint::{BigInt, Sign};

//===========================================================================//

/// The contents of a single lexical token.
#[derive(Debug, Eq, PartialEq)]
pub enum TokenValue {
    /// A "`:`" symbol.
    Colon,
    /// A "`,`" symbol.
    Comma,
    /// An identifier or keyword.
    Identifier(String),
    /// An integer literal.
    IntLiteral(BigInt),
    /// A linebreak (that wasn't suppressed, e.g. by a backslash).
    Linebreak,
}

impl TokenValue {
    /// Returns the human-readable name for this kind of token.
    pub fn name(&self) -> &str {
        match &self {
            TokenValue::Colon => "colon",
            TokenValue::Comma => "comma",
            TokenValue::Identifier(_) => "identifier",
            TokenValue::IntLiteral(_) => "int literal",
            TokenValue::Linebreak => "linebreak",
        }
    }
}

//===========================================================================//

/// A single lexical token, including location information.
#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    /// The locaiton in the file of the start of the token.
    pub start: SrcLoc,
    /// The contents of the token.
    pub value: TokenValue,
}

/// A fatal error encountered while tokenizing a source code file.
#[derive(Debug, Eq, PartialEq)]
pub struct LexError {
    /// The location in the file where the error occurred.
    pub location: SrcLoc,
    /// The error message to report to the user.
    pub message: String,
}

/// A partial result from a stream of lexer tokens.
pub type LexResult = StreamResult<Token, LexError>;

//===========================================================================//

enum LexState {
    /// The lexer is in the middle of a source code comment.
    Comment,
    /// The lexer is in the middle of a decimal integer literal.
    DecimalLiteral { start: SrcLoc, digits: Vec<u8> },
    /// The lexer is in the middle of an identifier token.
    Identifier { start: SrcLoc, chars: Vec<u8> },
    /// The lexer is consuming whitespace between tokens.
    Whitespace,
    /// The lexer already reported an error, and cannot yield any more tokens.
    Error,
}

/// A lexer for tokenizing an input file.
pub struct TokenLexer {
    loc: SrcLoc,
    state: LexState,
    backslash: Option<SrcLoc>,
}

impl TokenLexer {
    /// Constructs a new lexer in its initial state.
    #[allow(clippy::new_without_default)]
    pub fn new() -> TokenLexer {
        TokenLexer {
            loc: SrcLoc::start(),
            state: LexState::Whitespace,
            backslash: None,
        }
    }

    /// Given the next chunk of input bytes, returns the number of bytes
    /// consumed and the next token in the stream (if any).  Pass an empty
    /// buffer to indicate the end of input.
    pub fn read_from(&mut self, buffer: &[u8]) -> (usize, LexResult) {
        if buffer.is_empty() { self.eof() } else { self.consume_input(buffer) }
    }

    fn consume_input(&mut self, buffer: &[u8]) -> (usize, LexResult) {
        for (offset, &byte) in buffer.iter().enumerate() {
            match &mut self.state {
                LexState::Error => {
                    return self.already_in_error_state();
                }
                LexState::Whitespace => match byte {
                    b' ' | b'\t' => {}
                    b'\n' => {
                        if self.backslash.is_some() {
                            self.backslash = None;
                            self.loc.newline();
                            continue;
                        }
                        let start = self.loc;
                        let value = TokenValue::Linebreak;
                        self.loc.newline();
                        return self.token_at(offset + 1, start, value);
                    }
                    b'\\' => {
                        self.backslash = Some(self.loc);
                    }
                    b';' => {
                        self.state = LexState::Comment;
                    }
                    b':' => {
                        let start = self.loc;
                        let value = TokenValue::Colon;
                        self.loc.column += 1;
                        return self.token_at(offset + 1, start, value);
                    }
                    b',' => {
                        let start = self.loc;
                        let value = TokenValue::Comma;
                        self.loc.column += 1;
                        return self.token_at(offset + 1, start, value);
                    }
                    b'0'..=b'9' => {
                        self.state = LexState::DecimalLiteral {
                            start: self.loc,
                            digits: vec![byte - b'0'],
                        };
                    }
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                        self.state = LexState::Identifier {
                            start: self.loc,
                            chars: vec![byte],
                        };
                    }
                    _ => {
                        return self
                            .error(format!("unexpected byte: {byte:?}"));
                    }
                },
                LexState::Comment => {
                    if byte == b'\n' {
                        self.state = LexState::Whitespace;
                        if self.backslash.is_some() {
                            self.backslash = None;
                            self.loc.newline();
                            continue;
                        }
                        let start = self.loc;
                        let value = TokenValue::Linebreak;
                        self.loc.newline();
                        return self.token_at(offset + 1, start, value);
                    }
                }
                LexState::DecimalLiteral { start, digits } => match byte {
                    b'0'..=b'9' => {
                        digits.push(byte - b'0');
                    }
                    _ => {
                        let start = *start;
                        let integer =
                            BigInt::from_radix_be(Sign::Plus, digits, 10)
                                .unwrap();
                        let value = TokenValue::IntLiteral(integer);
                        self.state = LexState::Whitespace;
                        return self.token_at(offset, start, value);
                    }
                },
                LexState::Identifier { start, chars } => match byte {
                    b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_' => {
                        chars.push(byte);
                    }
                    _ => {
                        let start = *start;
                        let id =
                            String::from_utf8(std::mem::take(chars)).unwrap();
                        let value = TokenValue::Identifier(id);
                        self.state = LexState::Whitespace;
                        return self.token_at(offset, start, value);
                    }
                },
            }
            self.loc.column += 1;
        }
        // At this point, we've consumed the whole (non-empty) buffer without
        // producing a token, so more input is needed.
        (buffer.len(), LexResult::NeedMoreInput)
    }

    fn eof(&mut self) -> (usize, LexResult) {
        match std::mem::replace(&mut self.state, LexState::Whitespace) {
            LexState::Error => self.already_in_error_state(),
            LexState::Whitespace | LexState::Comment => {
                if let Some(location) = self.backslash {
                    self.error_at(
                        location,
                        "hanging backslash before EOF".to_string(),
                    )
                } else {
                    (0, LexResult::NoMoreOutput)
                }
            }
            LexState::DecimalLiteral { start, digits } => {
                let integer =
                    BigInt::from_radix_be(Sign::Plus, &digits, 10).unwrap();
                let value = TokenValue::IntLiteral(integer);
                self.state = LexState::Whitespace;
                self.token_at(0, start, value)
            }
            LexState::Identifier { start, chars } => {
                let id = String::from_utf8(chars).unwrap();
                let value = TokenValue::Identifier(id);
                self.state = LexState::Whitespace;
                self.token_at(0, start, value)
            }
        }
    }

    fn token_at(
        &mut self,
        offset: usize,
        start: SrcLoc,
        value: TokenValue,
    ) -> (usize, LexResult) {
        if let Some(location) = self.backslash {
            self.error_at(
                location,
                format!("unexpected backslash before {}", value.name()),
            )
        } else {
            (offset, LexResult::Yield(Token { start, value }))
        }
    }

    /// Puts the [`TokenLexer`] into a fatal error state, and returns an error
    /// result at the current location (without consuming any input).
    fn error(&mut self, message: String) -> (usize, LexResult) {
        self.error_at(self.loc, message)
    }

    /// Puts the [`TokenLexer`] into a fatal error state, and returns an error
    /// result at the specified location (without consuming any input).
    fn error_at(
        &mut self,
        location: SrcLoc,
        message: String,
    ) -> (usize, LexResult) {
        self.state = LexState::Error;
        self.backslash = None;
        (0, LexResult::Error(LexError { location, message }))
    }

    /// Returns an error result indicating that the [`TokenLexer`] was already
    /// in a fatal error state.
    fn already_in_error_state(&mut self) -> (usize, LexResult) {
        self.error("cannot yield more tokens after an error".to_string())
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{LexError, LexResult, Token, TokenLexer, TokenValue};
    use crate::parse::SrcLoc;
    use num_bigint::BigInt;

    fn token(line: u32, column: usize, value: TokenValue) -> Token {
        Token { start: SrcLoc { line, column }, value }
    }

    fn error(line: u32, column: usize, message: &str) -> LexError {
        LexError {
            location: SrcLoc { line, column },
            message: message.to_string(),
        }
    }

    fn read_all(mut input: &[u8]) -> Vec<Token> {
        let mut lexer = TokenLexer::new();
        let mut tokens = Vec::<Token>::new();
        loop {
            let (consumed, result) = lexer.read_from(input);
            match result {
                LexResult::NeedMoreInput => {
                    if input.is_empty() {
                        panic!("NeedMoreInput");
                    }
                }
                LexResult::Yield(token) => tokens.push(token),
                LexResult::NoMoreOutput => break,
                LexResult::Error(error) => panic!("Error({error:?})"),
            }
            input = &input[consumed..];
        }
        tokens
    }

    fn expect_error(mut input: &[u8]) -> LexError {
        let mut lexer = TokenLexer::new();
        loop {
            let (consumed, result) = lexer.read_from(input);
            match result {
                LexResult::NeedMoreInput => {
                    if input.is_empty() {
                        panic!("NeedMoreInput");
                    }
                }
                LexResult::Yield(_) => {}
                LexResult::NoMoreOutput => panic!("NoMoreOutput"),
                LexResult::Error(error) => return error,
            }
            input = &input[consumed..];
        }
    }

    #[test]
    fn empty_input() {
        assert_eq!(read_all(b""), vec![]);
    }

    #[test]
    fn comment() {
        assert_eq!(read_all(b";;; Hello, world!"), vec![]);
    }

    #[test]
    fn linebreak() {
        assert_eq!(read_all(b"\n"), vec![token(1, 0, TokenValue::Linebreak)]);
    }

    #[test]
    fn decimal_literal() {
        assert_eq!(
            read_all(b"12345"),
            vec![token(1, 0, TokenValue::IntLiteral(BigInt::from(12345)))]
        );
    }

    #[test]
    fn comma_separated_integers() {
        assert_eq!(
            read_all(b"1, 2, 3"),
            vec![
                token(1, 0, TokenValue::IntLiteral(BigInt::from(1))),
                token(1, 1, TokenValue::Comma),
                token(1, 3, TokenValue::IntLiteral(BigInt::from(2))),
                token(1, 4, TokenValue::Comma),
                token(1, 6, TokenValue::IntLiteral(BigInt::from(3))),
            ]
        );
    }

    #[test]
    fn backslash_before_linebreak() {
        assert_eq!(
            read_all(b"  \\  \n42"),
            vec![token(2, 0, TokenValue::IntLiteral(BigInt::from(42)))]
        );
    }

    #[test]
    fn backslash_before_comment() {
        assert_eq!(
            read_all(b"  \\ ; 123 \n:"),
            vec![token(2, 0, TokenValue::Colon)]
        );
    }

    #[test]
    fn backslash_before_eof() {
        assert_eq!(
            expect_error(b"  \\ "),
            error(1, 2, "hanging backslash before EOF")
        );
    }

    #[test]
    fn backslash_before_identifier() {
        assert_eq!(
            expect_error(b"  \\ foo"),
            error(1, 2, "unexpected backslash before identifier")
        );
    }
}

//===========================================================================//
