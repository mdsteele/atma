use crate::parse::{ParseError, SrcSpan};
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
    backslash: Option<SrcSpan>,
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
    if let Some(span) = lexer.extras.backslash {
        let message = "unexpected backslash before backslash".to_string();
        Err(ParseError::new(span, message))
    } else {
        lexer.extras.backslash = Some(SrcSpan::from_byte_range(lexer.span()));
        Ok(logos::Skip)
    }
}

fn binary_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> BigInt {
    debug_assert!(lex.slice().starts_with("%"));
    let digits: Vec<u8> =
        lex.slice()[1..].chars().map(|chr| chr as u8 - b'0').collect();
    BigInt::from_radix_be(Sign::Plus, &digits, 2).unwrap()
}

fn decimal_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> BigInt {
    let digits: Vec<u8> =
        lex.slice().chars().map(|chr| chr as u8 - b'0').collect();
    BigInt::from_radix_be(Sign::Plus, &digits, 10).unwrap()
}

fn hex_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> BigInt {
    debug_assert!(lex.slice().starts_with("$"));
    let digits: Vec<u8> = lex.slice()[1..]
        .chars()
        .map(|chr| {
            let byte = chr as u8;
            match byte {
                b'A'..=b'F' => (byte - b'A') + 10,
                b'a'..=b'f' => (byte - b'a') + 10,
                _ => byte - b'0',
            }
        })
        .collect();
    BigInt::from_radix_be(Sign::Plus, &digits, 16).unwrap()
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

fn string_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> String {
    debug_assert!(lex.slice().starts_with("\""));
    debug_assert!(lex.slice().ends_with("\""));
    let mut string = String::new();
    let mut backslash = false;
    for chr in lex.slice()[1..(lex.slice().len() - 1)].chars() {
        if backslash {
            string.push(match chr {
                'n' => '\n',
                't' => '\t',
                _ => chr, // TODO return error for invalid escape
            });
            backslash = false;
        } else if chr == '\\' {
            backslash = true;
        } else {
            string.push(chr);
        }
    }
    string
}

#[derive(Debug, Eq, Logos, PartialEq)]
#[logos(error = LexerError)]
#[logos(extras = LexerState)]
#[logos(skip r"[ \t]+")] // whitespace
#[logos(skip r";[^\n]*")] // comments
#[logos(source = str)]
enum TokenKind {
    #[token("\\", backslash_callback)]
    Backslash,
    #[token("%false")]
    BoolLiteralFalse,
    #[token("%true")]
    BoolLiteralTrue,
    #[token("}")]
    BraceClose,
    #[token("{")]
    BraceOpen,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("==")]
    EqEq,
    #[token("=")]
    Equals,
    #[regex(r"[_A-Za-z][_A-Za-z0-9]*")]
    Identifier,
    #[regex(r"%[01]+", binary_literal_callback)]
    #[regex(r"[0-9]+", decimal_literal_callback)]
    #[regex(r"\$[0-9A-Fa-f]+", hex_literal_callback)]
    IntLiteral(BigInt),
    #[regex(r"\n", newline_callback)]
    Linebreak,
    #[token(")")]
    ParenClose,
    #[token("(")]
    ParenOpen,
    #[token("+")]
    Plus,
    #[regex("\"([^\"\\n\\\\]|\\\\.)+\"", string_literal_callback)]
    StrLiteral(String),
}

impl TokenKind {
    fn into_token(
        self,
        lexer: &logos::Lexer<TokenKind>,
    ) -> Result<Token, ParseError> {
        let span = SrcSpan::from_byte_range(lexer.span());
        let value = match self {
            TokenKind::Backslash => unreachable!(),
            TokenKind::BoolLiteralFalse => TokenValue::BoolLiteral(false),
            TokenKind::BoolLiteralTrue => TokenValue::BoolLiteral(true),
            TokenKind::BraceClose => TokenValue::BraceClose,
            TokenKind::BraceOpen => TokenValue::BraceOpen,
            TokenKind::Colon => TokenValue::Colon,
            TokenKind::Comma => TokenValue::Comma,
            TokenKind::EqEq => TokenValue::EqEq,
            TokenKind::Equals => TokenValue::Equals,
            TokenKind::Identifier => {
                TokenValue::Identifier(lexer.slice().to_string())
            }
            TokenKind::IntLiteral(int) => TokenValue::IntLiteral(int),
            TokenKind::Linebreak => TokenValue::Linebreak,
            TokenKind::ParenClose => TokenValue::ParenClose,
            TokenKind::ParenOpen => TokenValue::ParenOpen,
            TokenKind::Plus => TokenValue::Plus,
            TokenKind::StrLiteral(string) => TokenValue::StrLiteral(string),
        };
        if let Some(span) = lexer.extras.backslash {
            let message =
                format!("unexpected backslash before {}", value.name());
            return Err(ParseError::new(span, message));
        }
        Ok(Token { span, value })
    }
}

//===========================================================================//

/// The contents of a single lexical token.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenValue {
    /// An boolean literal.
    BoolLiteral(bool),
    /// A "`}`" symbol.
    BraceClose,
    /// A "`{`" symbol.
    BraceOpen,
    /// A "`:`" symbol.
    Colon,
    /// A "`,`" symbol.
    Comma,
    /// A "`==`" symbol.
    EqEq,
    /// A "`=`" symbol.
    Equals,
    /// An identifier or keyword.
    Identifier(String),
    /// An integer literal.
    IntLiteral(BigInt),
    /// A linebreak (that wasn't suppressed, e.g. by a backslash).
    Linebreak,
    /// A "`)`" symbol.
    ParenClose,
    /// A "`(`" symbol.
    ParenOpen,
    /// A "`+`" symbol.
    Plus,
    /// A string literal.
    StrLiteral(String),
}

impl TokenValue {
    /// Returns the human-readable name for this kind of token.
    pub fn name(&self) -> &'static str {
        match &self {
            TokenValue::BoolLiteral(_) => "boolean literal",
            TokenValue::BraceClose => "close brace",
            TokenValue::BraceOpen => "open brace",
            TokenValue::Colon => "colon",
            TokenValue::Comma => "comma",
            TokenValue::EqEq => "equals-equals",
            TokenValue::Equals => "equals sign",
            TokenValue::Identifier(_) => "identifier",
            TokenValue::IntLiteral(_) => "integer literal",
            TokenValue::Linebreak => "linebreak",
            TokenValue::ParenClose => "close parenthesis",
            TokenValue::ParenOpen => "open parenthesis",
            TokenValue::Plus => "plus sign",
            TokenValue::StrLiteral(_) => "string literal",
        }
    }
}

//===========================================================================//

/// A single lexical token, including location information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    /// The locaiton in the file of the start of the token.
    pub span: SrcSpan,
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
    pub fn new(input: &'a str) -> TokenLexer<'a> {
        TokenLexer { lexer: TokenKind::lexer(input) }
    }
}

impl<'a> Iterator for TokenLexer<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Result<Token, ParseError>> {
        match self.lexer.next() {
            None => {
                if let Some(span) = self.lexer.extras.backslash {
                    self.lexer.extras.backslash = None;
                    let message =
                        "unexpected backslash before EOF".to_string();
                    Some(Err(ParseError::new(span, message)))
                } else {
                    None
                }
            }
            Some(Err(LexerError::ParseError(error))) => Some(Err(error)),
            Some(Err(LexerError::InvalidToken)) => {
                let span = SrcSpan::from_byte_range(self.lexer.span());
                let message = format!(
                    "invalid character: '{}'",
                    self.lexer.slice().escape_debug()
                );
                Some(Err(ParseError::new(span, message)))
            }
            Some(Ok(kind)) => Some(kind.into_token(&self.lexer)),
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ParseError, Token, TokenLexer, TokenValue};
    use crate::parse::SrcSpan;
    use num_bigint::BigInt;
    use std::ops::Range;

    fn token(range: Range<usize>, value: TokenValue) -> Token {
        Token { span: SrcSpan::from_byte_range(range), value }
    }

    fn error(range: Range<usize>, message: &str) -> ParseError {
        ParseError::new(SrcSpan::from_byte_range(range), message.to_string())
    }

    fn read_all(input: &str) -> Vec<Token> {
        TokenLexer::new(input).collect::<Result<_, _>>().unwrap()
    }

    fn expect_error(input: &str) -> ParseError {
        for result in TokenLexer::new(input) {
            if let Err(error) = result {
                return error;
            }
        }
        panic!("no error occurred");
    }

    #[test]
    fn empty_input() {
        assert_eq!(read_all(""), vec![]);
    }

    #[test]
    fn comment() {
        assert_eq!(read_all(";;; Hello, world!"), vec![]);
    }

    #[test]
    fn linebreak() {
        assert_eq!(read_all("\n"), vec![token(0..1, TokenValue::Linebreak)]);
    }

    #[test]
    fn binary_literal() {
        assert_eq!(
            read_all("%11010100"),
            vec![token(
                0..9,
                TokenValue::IntLiteral(BigInt::from(0b11010100))
            )]
        );
    }

    #[test]
    fn decimal_literal() {
        assert_eq!(
            read_all("12345"),
            vec![token(0..5, TokenValue::IntLiteral(BigInt::from(12345)))]
        );
    }

    #[test]
    fn hex_literal() {
        assert_eq!(
            read_all("$f0FA9a"),
            vec![token(0..7, TokenValue::IntLiteral(BigInt::from(0xf0fa9a)))]
        );
    }

    #[test]
    fn string_literal() {
        assert_eq!(
            read_all("\"foo\\n\\t\\\\\\\"\""),
            vec![token(
                0..13,
                TokenValue::StrLiteral("foo\n\t\\\"".to_string())
            )]
        );
    }

    #[test]
    fn comma_separated_integers() {
        assert_eq!(
            read_all("1, 2, 3"),
            vec![
                token(0..1, TokenValue::IntLiteral(BigInt::from(1))),
                token(1..2, TokenValue::Comma),
                token(3..4, TokenValue::IntLiteral(BigInt::from(2))),
                token(4..5, TokenValue::Comma),
                token(6..7, TokenValue::IntLiteral(BigInt::from(3))),
            ]
        );
    }

    #[test]
    fn backslash_before_linebreak() {
        assert_eq!(
            read_all("  \\  \n42"),
            vec![token(6..8, TokenValue::IntLiteral(BigInt::from(42)))]
        );
    }

    #[test]
    fn backslash_before_comment() {
        assert_eq!(
            read_all("  \\ ; 123 \n:"),
            vec![token(11..12, TokenValue::Colon)]
        );
    }

    #[test]
    fn backslash_before_backslash() {
        assert_eq!(
            expect_error("\\ \\ \n"),
            error(0..1, "unexpected backslash before backslash")
        );
    }

    #[test]
    fn backslash_before_eof() {
        assert_eq!(
            expect_error("  \\ "),
            error(2..3, "unexpected backslash before EOF")
        );
    }

    #[test]
    fn backslash_before_identifier() {
        assert_eq!(
            expect_error("  \\ foo"),
            error(2..3, "unexpected backslash before identifier")
        );
    }

    #[test]
    fn invalid_token() {
        assert_eq!(
            expect_error(" `foo\n"),
            error(1..2, "invalid character: '`'")
        );
    }
}

//===========================================================================//
