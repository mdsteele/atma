use crate::parse::{ParseError, SrcSpan};
use logos::{self, Logos};
use num_bigint::{BigInt, Sign};
use std::rc::Rc;

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

fn string_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> Rc<str> {
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
    Rc::from(string)
}

#[derive(Debug, Eq, Logos, PartialEq)]
#[logos(error = LexerError)]
#[logos(extras = LexerState)]
#[logos(skip r"[ \t]+")] // whitespace
#[logos(skip r";[^\n]*")] // comments
#[logos(utf8 = true)]
enum TokenKind {
    #[token("&")]
    And,
    #[token("&&")]
    AndAnd,
    #[token("\\", backslash_callback)]
    Backslash,
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEquals,
    #[token("%false")]
    BoolLiteralFalse,
    #[token("%true")]
    BoolLiteralTrue,
    #[token("}")]
    BraceClose,
    #[token("{")]
    BraceOpen,
    #[token("]")]
    BracketClose,
    #[token("[")]
    BracketOpen,
    #[token("^")]
    Caret,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[regex(r"\.[_A-Za-z][_A-Za-z0-9]*")]
    Directive,
    #[token("==")]
    EqualsEquals,
    #[token("=")]
    Equals,
    #[token(">=")]
    GreaterEquals,
    #[token(">>")]
    GreaterGreater,
    #[token(">")]
    GreaterThan,
    #[regex(r"[_A-Za-z][_A-Za-z0-9]*")]
    Identifier,
    #[regex(r"%[01]+", binary_literal_callback)]
    #[regex(r"[0-9]+", decimal_literal_callback)]
    #[regex(r"\$[0-9A-Fa-f]+", hex_literal_callback)]
    IntLiteral(BigInt),
    #[token("<=")]
    LessEquals,
    #[token("<<")]
    LessLess,
    #[token("<")]
    LessThan,
    #[regex(r"\n", newline_callback)]
    Linebreak,
    #[token("-")]
    Minus,
    #[token("|")]
    Or,
    #[token("||")]
    OrOr,
    #[token(")")]
    ParenClose,
    #[token("(")]
    ParenOpen,
    #[token("%")]
    Percent,
    #[token("+")]
    Plus,
    #[token("?")]
    Question,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token("**")]
    StarStar,
    #[regex("\"([^\"\\n\\\\]|\\\\.)+\"", string_literal_callback)]
    StrLiteral(Rc<str>),
    #[token("~")]
    Tilde,
}

impl TokenKind {
    fn into_token(
        self,
        lexer: &logos::Lexer<TokenKind>,
    ) -> Result<Token, ParseError> {
        let span = SrcSpan::from_byte_range(lexer.span());
        let value = match self {
            TokenKind::And => TokenValue::And,
            TokenKind::AndAnd => TokenValue::AndAnd,
            TokenKind::Backslash => unreachable!(),
            TokenKind::Bang => TokenValue::Bang,
            TokenKind::BangEquals => TokenValue::BangEquals,
            TokenKind::BoolLiteralFalse => TokenValue::BoolLiteral(false),
            TokenKind::BoolLiteralTrue => TokenValue::BoolLiteral(true),
            TokenKind::BraceClose => TokenValue::BraceClose,
            TokenKind::BraceOpen => TokenValue::BraceOpen,
            TokenKind::BracketClose => TokenValue::BracketClose,
            TokenKind::BracketOpen => TokenValue::BracketOpen,
            TokenKind::Caret => TokenValue::Caret,
            TokenKind::Colon => TokenValue::Colon,
            TokenKind::Comma => TokenValue::Comma,
            TokenKind::Directive => {
                TokenValue::Directive(Rc::from(lexer.slice()))
            }
            TokenKind::GreaterEquals => TokenValue::GreaterEquals,
            TokenKind::GreaterGreater => TokenValue::GreaterGreater,
            TokenKind::GreaterThan => TokenValue::GreaterThan,
            TokenKind::EqualsEquals => TokenValue::EqualsEquals,
            TokenKind::Equals => TokenValue::Equals,
            TokenKind::Identifier => {
                TokenValue::Identifier(Rc::from(lexer.slice()))
            }
            TokenKind::IntLiteral(int) => TokenValue::IntLiteral(int),
            TokenKind::LessEquals => TokenValue::LessEquals,
            TokenKind::LessLess => TokenValue::LessLess,
            TokenKind::LessThan => TokenValue::LessThan,
            TokenKind::Linebreak => TokenValue::Linebreak,
            TokenKind::Minus => TokenValue::Minus,
            TokenKind::Or => TokenValue::Or,
            TokenKind::OrOr => TokenValue::OrOr,
            TokenKind::ParenClose => TokenValue::ParenClose,
            TokenKind::ParenOpen => TokenValue::ParenOpen,
            TokenKind::Percent => TokenValue::Percent,
            TokenKind::Plus => TokenValue::Plus,
            TokenKind::Question => TokenValue::Question,
            TokenKind::Slash => TokenValue::Slash,
            TokenKind::Star => TokenValue::Star,
            TokenKind::StarStar => TokenValue::StarStar,
            TokenKind::StrLiteral(string) => TokenValue::StrLiteral(string),
            TokenKind::Tilde => TokenValue::Tilde,
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
    /// A "`&`" symbol.
    And,
    /// A "`&&`" symbol.
    AndAnd,
    /// A "`!`" symbol.
    Bang,
    /// A "`!=`" symbol.
    BangEquals,
    /// An boolean literal.
    BoolLiteral(bool),
    /// A "`}`" symbol.
    BraceClose,
    /// A "`{`" symbol.
    BraceOpen,
    /// A "`]`" symbol.
    BracketClose,
    /// A "`[`" symbol.
    BracketOpen,
    /// A "`^`" symbol.
    Caret,
    /// A "`:`" symbol.
    Colon,
    /// A "`,`" symbol.
    Comma,
    /// An assembler directive.
    Directive(Rc<str>),
    /// A "`==`" symbol.
    EqualsEquals,
    /// A "`=`" symbol.
    Equals,
    /// A "`>=`" symbol.
    GreaterEquals,
    /// A "`>>`" symbol.
    GreaterGreater,
    /// A "`>`" symbol.
    GreaterThan,
    /// An identifier or keyword.
    Identifier(Rc<str>),
    /// An integer literal.
    IntLiteral(BigInt),
    /// A "`<=`" symbol.
    LessEquals,
    /// A "`<<`" symbol.
    LessLess,
    /// A "`<`" symbol.
    LessThan,
    /// A linebreak (that wasn't suppressed, e.g. by a backslash).
    Linebreak,
    /// A "`-`" symbol.
    Minus,
    /// A "`|`" symbol.
    Or,
    /// A "`||`" symbol.
    OrOr,
    /// A "`)`" symbol.
    ParenClose,
    /// A "`(`" symbol.
    ParenOpen,
    /// A "`%`" symbol.
    Percent,
    /// A "`+`" symbol.
    Plus,
    /// A "`?`" symbol.
    Question,
    /// A "`/`" symbol.
    Slash,
    /// A "`*`" symbol.
    Star,
    /// A "`**`" symbol.
    StarStar,
    /// A string literal.
    StrLiteral(Rc<str>),
    /// A "`~`" symbol.
    Tilde,
}

impl TokenValue {
    /// Returns the human-readable name for this kind of token.
    pub fn name(&self) -> &'static str {
        match &self {
            TokenValue::And => "`&`",
            TokenValue::AndAnd => "`&&`",
            TokenValue::Bang => "`!`",
            TokenValue::BangEquals => "`!=`",
            TokenValue::BoolLiteral(_) => "boolean literal",
            TokenValue::BraceClose => "close brace",
            TokenValue::BraceOpen => "open brace",
            TokenValue::BracketClose => "close bracket",
            TokenValue::BracketOpen => "open bracket",
            TokenValue::Caret => "`^`",
            TokenValue::Colon => "colon",
            TokenValue::Comma => "comma",
            TokenValue::Directive(_) => "directive",
            TokenValue::EqualsEquals => "`==`",
            TokenValue::Equals => "`=`",
            TokenValue::GreaterEquals => "`>=`",
            TokenValue::GreaterGreater => "`>>`",
            TokenValue::GreaterThan => "`>`",
            TokenValue::Identifier(_) => "identifier",
            TokenValue::IntLiteral(_) => "integer literal",
            TokenValue::LessEquals => "`<=`",
            TokenValue::LessLess => "`<<`",
            TokenValue::LessThan => "`<`",
            TokenValue::Linebreak => "linebreak",
            TokenValue::Minus => "`-`",
            TokenValue::Or => "`|`",
            TokenValue::OrOr => "`||`",
            TokenValue::ParenClose => "close parenthesis",
            TokenValue::ParenOpen => "open parenthesis",
            TokenValue::Percent => "`%`",
            TokenValue::Plus => "`+`",
            TokenValue::Question => "`?`",
            TokenValue::Slash => "`/`",
            TokenValue::Star => "`*`",
            TokenValue::StarStar => "`**`",
            TokenValue::StrLiteral(_) => "string literal",
            TokenValue::Tilde => "`~`",
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
    use std::rc::Rc;

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
                TokenValue::StrLiteral(Rc::from("foo\n\t\\\""))
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
