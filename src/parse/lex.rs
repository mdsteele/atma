use crate::parse::{ParseError, SrcLoc};
use logos::{self, Logos};
use num_bigint::{BigInt, Sign};

//===========================================================================//

#[derive(Clone, Debug, Default, PartialEq)]
enum LexerError {
    #[default]
    InvalidToken,
    ParseError(ParseError),
}

impl std::convert::From<ParseError> for LexerError {
    fn from(value: ParseError) -> LexerError {
        LexerError::ParseError(value)
    }
}

//===========================================================================//

struct LexerState {
    line: u32,
    start_of_line: usize,
    backslash: Option<SrcLoc>,
}

impl Default for LexerState {
    fn default() -> LexerState {
        LexerState { line: 1, start_of_line: 0, backslash: None }
    }
}

//===========================================================================//

fn backslash_callback(
    lexer: &mut logos::Lexer<TokenKind>,
) -> Result<logos::Skip, ParseError> {
    if let Some(location) = lexer.extras.backslash {
        let message = "unexpected backslash before backslash".to_string();
        Err(ParseError { location, message })
    } else {
        lexer.extras.backslash = Some(TokenKind::Backslash.location(lexer));
        Ok(logos::Skip)
    }
}

fn binary_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> BigInt {
    let digits: Vec<u8> = lex.slice().iter().map(|&chr| chr - b'0').collect();
    BigInt::from_radix_be(Sign::Plus, &digits, 2).unwrap()
}

fn decimal_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> BigInt {
    let digits: Vec<u8> = lex.slice().iter().map(|&chr| chr - b'0').collect();
    BigInt::from_radix_be(Sign::Plus, &digits, 10).unwrap()
}

fn newline_callback(lexer: &mut logos::Lexer<TokenKind>) -> logos::Filter<()> {
    lexer.extras.line += 1;
    lexer.extras.start_of_line = lexer.span().end;
    if lexer.extras.backslash.is_some() {
        lexer.extras.backslash = None;
        logos::Filter::Skip
    } else {
        logos::Filter::Emit(())
    }
}

#[derive(Debug, Eq, Logos, PartialEq)]
#[logos(error = LexerError)]
#[logos(extras = LexerState)]
#[logos(skip r"[ \t]+")] // whitespace
#[logos(skip r";[^\n]+")] // comments
#[logos(source = [u8])]
enum TokenKind {
    #[token("\\", backslash_callback)]
    Backslash,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[regex(r"[_A-Za-z][_A-Za-z0-9]*")]
    Identifier,
    #[regex(r"%[01]+", binary_literal_callback)]
    #[regex(r"[0-9]+", decimal_literal_callback)]
    IntLiteral(BigInt),
    #[regex(r"\n", newline_callback)]
    Linebreak,
}

impl TokenKind {
    fn lexer_location(lexer: &logos::Lexer<TokenKind>) -> SrcLoc {
        SrcLoc {
            line: lexer.extras.line,
            column: lexer.span().start - lexer.extras.start_of_line,
        }
    }

    fn location(&self, lexer: &logos::Lexer<TokenKind>) -> SrcLoc {
        if let &TokenKind::Linebreak = self {
            SrcLoc {
                line: lexer.extras.line - 1,
                column: lexer.extras.start_of_line - 1,
            }
        } else {
            TokenKind::lexer_location(lexer)
        }
    }

    fn into_token(
        self,
        lexer: &logos::Lexer<TokenKind>,
    ) -> Result<Token, ParseError> {
        let start = self.location(lexer);
        let value = match self {
            TokenKind::Backslash => unreachable!(),
            TokenKind::Colon => TokenValue::Colon,
            TokenKind::Comma => TokenValue::Comma,
            TokenKind::Identifier => {
                let id = String::from_utf8(lexer.slice().to_vec()).unwrap();
                TokenValue::Identifier(id)
            }
            TokenKind::IntLiteral(int) => TokenValue::IntLiteral(int),
            TokenKind::Linebreak => TokenValue::Linebreak,
        };
        if let Some(location) = lexer.extras.backslash {
            let message =
                format!("unexpected backslash before {}", value.name());
            return Err(ParseError { location, message });
        }
        Ok(Token { start, value })
    }
}

//===========================================================================//

/// The contents of a single lexical token.
#[derive(Clone, Debug, Eq, PartialEq)]
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    /// The locaiton in the file of the start of the token.
    pub start: SrcLoc,
    /// The contents of the token.
    pub value: TokenValue,
}

//===========================================================================//

/// A lexer for tokenizing an input file.
pub struct TokenLexer<'a> {
    lexer: logos::Lexer<'a, TokenKind>,
}

impl<'a> TokenLexer<'a> {
    /// Constructs a new lexer in its initial state.
    pub fn new(input: &[u8]) -> TokenLexer {
        TokenLexer { lexer: TokenKind::lexer(input) }
    }
}

impl<'a> Iterator for TokenLexer<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Result<Token, ParseError>> {
        match self.lexer.next() {
            None => {
                if let Some(location) = self.lexer.extras.backslash {
                    self.lexer.extras.backslash = None;
                    let message =
                        "unexpected backslash before EOF".to_string();
                    Some(Err(ParseError { location, message }))
                } else {
                    None
                }
            }
            Some(Err(LexerError::ParseError(error))) => Some(Err(error)),
            Some(Err(LexerError::InvalidToken)) => {
                let location = TokenKind::lexer_location(&self.lexer);
                let message = format!(
                    "invalid character: {}",
                    self.lexer.slice().escape_ascii()
                );
                Some(Err(ParseError { location, message }))
            }
            Some(Ok(kind)) => Some(kind.into_token(&self.lexer)),
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ParseError, Token, TokenLexer, TokenValue};
    use crate::parse::SrcLoc;
    use num_bigint::BigInt;

    fn token(line: u32, column: usize, value: TokenValue) -> Token {
        Token { start: SrcLoc { line, column }, value }
    }

    fn error(line: u32, column: usize, message: &str) -> ParseError {
        ParseError {
            location: SrcLoc { line, column },
            message: message.to_string(),
        }
    }

    fn read_all(input: &[u8]) -> Vec<Token> {
        TokenLexer::new(input).collect::<Result<_, _>>().unwrap()
    }

    fn expect_error(input: &[u8]) -> ParseError {
        for result in TokenLexer::new(input) {
            if let Err(error) = result {
                return error;
            }
        }
        panic!("no error occurred");
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
    fn backslash_before_backslash() {
        assert_eq!(
            expect_error(b"\\ \\ \n"),
            error(1, 0, "unexpected backslash before backslash")
        );
    }

    #[test]
    fn backslash_before_eof() {
        assert_eq!(
            expect_error(b"  \\ "),
            error(1, 2, "unexpected backslash before EOF")
        );
    }

    #[test]
    fn backslash_before_identifier() {
        assert_eq!(
            expect_error(b"  \\ foo"),
            error(1, 2, "unexpected backslash before identifier")
        );
    }

    #[test]
    fn invalid_token() {
        assert_eq!(
            expect_error(b" `foo\n"),
            error(1, 1, "invalid character: `")
        );
    }
}

//===========================================================================//
