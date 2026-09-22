use super::error::{LexerError, LogosLexerError};
use super::string::{hex_digit_value, unescape_string_literal};
use super::token::{Token, TokenValue};
use crate::error::SrcSpan;
use logos::{self, Logos};
use num_bigint::{BigInt, Sign};
use std::rc::Rc;

//===========================================================================//

#[derive(Default)]
struct LexerState {
    backslash: Option<SrcSpan>,
}

//===========================================================================//

fn backslash_callback(
    lexer: &mut logos::Lexer<TokenKind>,
) -> Result<logos::Skip, LexerError> {
    let span = SrcSpan::from_byte_range(lexer.span());
    if let Some(previous_span) = lexer.extras.backslash {
        Err(LexerError::BackslashBeforeBackslash(previous_span, span))
    } else {
        lexer.extras.backslash = Some(span);
        Ok(logos::Skip)
    }
}

fn binary_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> BigInt {
    debug_assert!(lex.slice().starts_with("%"));
    let digits: Vec<u8> = lex.slice()[1..]
        .chars()
        .filter(|chr| *chr != '_')
        .map(|chr| chr as u8 - b'0')
        .collect();
    BigInt::from_radix_be(Sign::Plus, &digits, 2).unwrap()
}

fn decimal_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> BigInt {
    let digits: Vec<u8> = lex
        .slice()
        .chars()
        .filter(|chr| *chr != '_')
        .map(|chr| chr as u8 - b'0')
        .collect();
    BigInt::from_radix_be(Sign::Plus, &digits, 10).unwrap()
}

fn hex_literal_callback(lex: &mut logos::Lexer<TokenKind>) -> BigInt {
    debug_assert!(lex.slice().starts_with("$"));
    let digits: Vec<u8> = lex.slice()[1..]
        .chars()
        .filter(|chr| *chr != '_')
        .map(|chr| hex_digit_value(chr).unwrap())
        .collect();
    BigInt::from_radix_be(Sign::Plus, &digits, 16).unwrap()
}

fn newline_callback(lex: &mut logos::Lexer<TokenKind>) -> logos::Filter<()> {
    if lex.extras.backslash.is_some() {
        lex.extras.backslash = None;
        logos::Filter::Skip
    } else {
        logos::Filter::Emit(())
    }
}

fn string_literal_callback(
    lex: &mut logos::Lexer<TokenKind>,
) -> Result<Rc<str>, LexerError> {
    debug_assert!(lex.slice().starts_with("\""));
    debug_assert!(lex.slice().ends_with("\""));
    let contents = &lex.slice()["\"".len()..(lex.slice().len() - "\"".len())];
    let contents_start = lex.span().start + "\"".len();
    unescape_string_literal(contents, contents_start)
}

fn error_callback(lexer: &mut logos::Lexer<TokenKind>) -> LexerError {
    let span = SrcSpan::from_byte_range(lexer.span());
    let text = Rc::from(lexer.slice());
    LexerError::UnrecognizedToken(span, text)
}

//===========================================================================//

#[derive(Debug, Eq, Logos, PartialEq)]
#[logos(error(LogosLexerError, callback = error_callback))]
#[logos(extras = LexerState)]
#[logos(skip r"[ \t]+")] // whitespace
#[logos(skip(r";[^\n]*", allow_greedy = true))] // comments
#[logos(utf8 = true)]
enum TokenKind {
    #[token("&")]
    And,
    #[token("&&")]
    AndAnd,
    #[token("<-")]
    ArrowLeft,
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
    #[regex(r"%[a-z][_a-z0-9]*")]
    Builtin,
    #[token("^")]
    Caret,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(",")]
    Comma,
    #[regex(r"\.[_A-Za-z][_A-Za-z0-9]*")]
    Directive,
    #[token("$v")]
    DollarDown,
    #[token("$<")]
    DollarLeft,
    #[token("$>")]
    DollarRight,
    #[token("$^")]
    DollarUp,
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
    #[regex(r"[A-Za-z][_A-Za-z0-9]*|_[_A-Za-z0-9]+")]
    Identifier,
    #[regex(r"%[01]+(_[01]+)*", binary_literal_callback)]
    #[regex(r"[0-9]+(_[0-9]+)*", decimal_literal_callback)]
    #[regex(r"\$[0-9A-Fa-f]+(_[0-9A-Fa-f]+)*", hex_literal_callback)]
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
    #[token("%%")]
    PercentPercent,
    #[regex(r"%[A-Z][_A-Z0-9]*")]
    Placeholder,
    #[token("+")]
    Plus,
    #[token("++")]
    PlusPlus,
    #[token("#")]
    Pound,
    #[token("?")]
    Question,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token("**")]
    StarStar,
    #[regex("\"([^\"\\n\\\\]|\\\\.)*\"", string_literal_callback)]
    StrLiteral(Rc<str>),
    #[token("~")]
    Tilde,
    #[token("_")]
    Underscore,
}

impl TokenKind {
    fn into_token(
        self,
        lexer: &logos::Lexer<TokenKind>,
    ) -> Result<Token, LexerError> {
        let token_span = SrcSpan::from_byte_range(lexer.span());
        let token_value = match self {
            TokenKind::And => TokenValue::And,
            TokenKind::AndAnd => TokenValue::AndAnd,
            TokenKind::ArrowLeft => TokenValue::ArrowLeft,
            TokenKind::Backslash => unreachable!(),
            TokenKind::Bang => TokenValue::Bang,
            TokenKind::BangEquals => TokenValue::BangEquals,
            TokenKind::BoolLiteralFalse => TokenValue::BoolLiteral(false),
            TokenKind::BoolLiteralTrue => TokenValue::BoolLiteral(true),
            TokenKind::BraceClose => TokenValue::BraceClose,
            TokenKind::BraceOpen => TokenValue::BraceOpen,
            TokenKind::BracketClose => TokenValue::BracketClose,
            TokenKind::BracketOpen => TokenValue::BracketOpen,
            TokenKind::Builtin => TokenValue::Builtin(Rc::from(lexer.slice())),
            TokenKind::Caret => TokenValue::Caret,
            TokenKind::Colon => TokenValue::Colon,
            TokenKind::ColonColon => TokenValue::ColonColon,
            TokenKind::Comma => TokenValue::Comma,
            TokenKind::Directive => {
                TokenValue::Directive(Rc::from(lexer.slice()))
            }
            TokenKind::DollarDown => TokenValue::DollarDown,
            TokenKind::DollarLeft => TokenValue::DollarLeft,
            TokenKind::DollarRight => TokenValue::DollarRight,
            TokenKind::DollarUp => TokenValue::DollarUp,
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
            TokenKind::Placeholder => {
                TokenValue::Placeholder(Rc::from(lexer.slice()))
            }
            TokenKind::Minus => TokenValue::Minus,
            TokenKind::Or => TokenValue::Or,
            TokenKind::OrOr => TokenValue::OrOr,
            TokenKind::ParenClose => TokenValue::ParenClose,
            TokenKind::ParenOpen => TokenValue::ParenOpen,
            TokenKind::Percent => TokenValue::Percent,
            TokenKind::PercentPercent => TokenValue::PercentPercent,
            TokenKind::Plus => TokenValue::Plus,
            TokenKind::PlusPlus => TokenValue::PlusPlus,
            TokenKind::Pound => TokenValue::Pound,
            TokenKind::Question => TokenValue::Question,
            TokenKind::Slash => TokenValue::Slash,
            TokenKind::Star => TokenValue::Star,
            TokenKind::StarStar => TokenValue::StarStar,
            TokenKind::StrLiteral(string) => TokenValue::StrLiteral(string),
            TokenKind::Tilde => TokenValue::Tilde,
            TokenKind::Underscore => TokenValue::Underscore,
        };
        let token = Token { span: token_span, value: token_value };
        if let Some(backslash_span) = lexer.extras.backslash {
            Err(LexerError::BackslashBeforeToken(backslash_span, token))
        } else {
            Ok(token)
        }
    }
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
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Result<Token, LexerError>> {
        match self.lexer.next() {
            None => {
                if let Some(span) = self.lexer.extras.backslash {
                    return Some(Err(LexerError::BackslashBeforeEof(span)));
                }
                None
            }
            Some(Ok(kind)) => Some(kind.into_token(&self.lexer)),
            Some(Err(error)) => Some(Err(error.0)),
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{LexerError, Token, TokenLexer, TokenValue};
    use crate::error::SrcSpan;
    use num_bigint::BigInt;
    use std::ops::Range;
    use std::rc::Rc;

    fn token(range: Range<usize>, value: TokenValue) -> Token {
        Token { span: SrcSpan::from_byte_range(range), value }
    }

    fn read_all(input: &str) -> Vec<Token> {
        TokenLexer::new(input).collect::<Result<_, _>>().unwrap()
    }

    fn expect_error(input: &str) -> LexerError {
        for result in TokenLexer::new(input) {
            if let Err(error) = result {
                return error;
            }
        }
        panic!("no error occurred");
    }

    fn assert_invalid_escape(input: &str, range: Range<usize>, escape: &str) {
        assert_eq!(
            expect_error(input),
            LexerError::InvalidStringEscape(
                SrcSpan::from_byte_range(range),
                Rc::from(escape)
            )
        );
    }

    #[test]
    fn empty_input() {
        assert_eq!(read_all(""), vec![]);
    }

    #[test]
    fn comment() {
        assert_eq!(read_all(";;; Hello, world!"), vec![]);
        assert_eq!(
            read_all(";\n!"),
            vec![
                token(1..2, TokenValue::Linebreak),
                token(2..3, TokenValue::Bang)
            ]
        );
    }

    #[test]
    fn linebreak() {
        assert_eq!(read_all("\n"), vec![token(0..1, TokenValue::Linebreak)]);
    }

    #[test]
    fn binary_literal() {
        assert_eq!(
            read_all("%1101_0100"),
            vec![token(
                0..10,
                TokenValue::IntLiteral(BigInt::from(0b11010100))
            )]
        );
    }

    #[test]
    fn bool_literal() {
        assert_eq!(
            read_all("%false"),
            vec![token(0..6, TokenValue::BoolLiteral(false))]
        );
        assert_eq!(
            read_all("%true"),
            vec![token(0..5, TokenValue::BoolLiteral(true))]
        );
    }

    #[test]
    fn builtin() {
        assert_eq!(
            read_all("%foo"),
            vec![token(0..4, TokenValue::Builtin(Rc::from("%foo")))]
        );
    }

    #[test]
    fn decimal_literal() {
        assert_eq!(
            read_all("12_345"),
            vec![token(0..6, TokenValue::IntLiteral(BigInt::from(12345)))]
        );
    }

    #[test]
    fn hex_literal() {
        assert_eq!(
            read_all("$00f0_FA9a"),
            vec![token(0..10, TokenValue::IntLiteral(BigInt::from(0xf0fa9a)))]
        );
    }

    #[test]
    fn placeholder() {
        assert_eq!(
            read_all("%ADDR"),
            vec![token(0..5, TokenValue::Placeholder(Rc::from("%ADDR")))]
        );
    }

    #[test]
    fn string_literal() {
        assert_eq!(
            read_all("\"\""),
            vec![token(0..2, TokenValue::StrLiteral(Rc::from("")))]
        );
        assert_eq!(
            read_all("\"foo\\n\\t\\\\\\\"\""),
            vec![token(
                0..13,
                TokenValue::StrLiteral(Rc::from("foo\n\t\\\""))
            )]
        );
        assert_eq!(
            read_all("\"\\0\""),
            vec![token(0..4, TokenValue::StrLiteral(Rc::from("\0")))]
        );
        assert_eq!(
            read_all("\"\\x7f\""),
            vec![token(0..6, TokenValue::StrLiteral(Rc::from("\x7f")))]
        );
        assert_eq!(
            read_all("\"\\u{1F602}\""),
            vec![token(0..11, TokenValue::StrLiteral(Rc::from("\u{1F602}")))]
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
            LexerError::BackslashBeforeBackslash(
                SrcSpan::from_byte_range(0..1),
                SrcSpan::from_byte_range(2..3)
            )
        );
    }

    #[test]
    fn backslash_before_eof() {
        assert_eq!(
            expect_error("  \\ "),
            LexerError::BackslashBeforeEof(SrcSpan::from_byte_range(2..3))
        );
    }

    #[test]
    fn backslash_before_identifier() {
        assert_eq!(
            expect_error("  \\ foo"),
            LexerError::BackslashBeforeToken(
                SrcSpan::from_byte_range(2..3),
                token(4..7, TokenValue::Identifier(Rc::from("foo")))
            )
        );
    }

    #[test]
    fn invalid_string_escape() {
        assert_invalid_escape("\"foo\\qbar\"", 4..6, "\\q");
        assert_invalid_escape("\"bar\\x3\"", 4..7, "\\x3");
        assert_invalid_escape("\"\\x5g\"", 1..5, "\\x5g");
        assert_invalid_escape("\"\\u\"", 1..3, "\\u");
        assert_invalid_escape("\"ab\\u{\"", 3..6, "\\u{");
        assert_invalid_escape("\"baz\\u{33\"", 4..9, "\\u{33");
        assert_invalid_escape("\"foo\\u{3g}bar\"", 4..10, "\\u{3g}");
    }

    #[test]
    fn invalid_token() {
        assert_eq!(
            expect_error(" `foo\n"),
            LexerError::UnrecognizedToken(
                SrcSpan::from_byte_range(1..2),
                Rc::from("`")
            )
        );
    }
}

//===========================================================================//
