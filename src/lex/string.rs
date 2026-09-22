use super::error::{LexerError, LogosLexerError};
use crate::error::SrcSpan;
use logos::{self, Logos};
use std::rc::Rc;

//===========================================================================//

#[derive(Logos)]
#[logos(error(LogosLexerError, callback = error_callback))]
#[logos(extras = usize)]
#[logos(utf8 = true)]
enum StringChar {
    #[token("\\\\", |_| '\\')]
    #[token("\\r", |_| '\r')]
    #[token("\\\"", |_| '"')]
    #[regex(r"\\x[0-9A-Fa-f][0-9A-Fa-f]", hex_callback)]
    #[token("\\n", |_| '\n')]
    #[token("\\0", |_| '\0')]
    #[token("\\'", |_| '\'')]
    #[token("\\t", |_| '\t')]
    #[regex(r"\\u\{[0-9A-Fa-f]+?\}", unicode_callback)]
    #[regex(r"\\x.?.?", invalid_callback)]
    #[regex(r"\\u\{[^\}]*?$", invalid_callback)]
    #[regex(r"\\u\{[^\}]*?\}", invalid_callback)]
    #[regex(r"\\.", invalid_callback, priority = 0)]
    EscapedChar(char),
    #[regex(r"[^\\]+")]
    LiteralChars,
}

fn hex_callback(lexer: &mut logos::Lexer<StringChar>) -> char {
    debug_assert!(lexer.slice().starts_with("\\x"));
    debug_assert_eq!(lexer.slice().len(), 4);
    let mut digits = lexer.slice()["\\x".len()..].chars();
    let digit1 = hex_digit_value(digits.next().unwrap()).unwrap();
    let digit2 = hex_digit_value(digits.next().unwrap()).unwrap();
    char::from((digit1 << 4) | digit2)
}

fn unicode_callback(
    lexer: &mut logos::Lexer<StringChar>,
) -> Result<char, LexerError> {
    let mut value: u32 = 0;
    debug_assert!(lexer.slice().starts_with("\\u{"));
    debug_assert!(lexer.slice().ends_with("}"));
    for digit in
        lexer.slice()["\\u{".len()..(lexer.slice().len() - "}".len())].chars()
    {
        value <<= 4;
        value |= u32::from(hex_digit_value(digit).unwrap());
        if value > u32::from(char::MAX) {
            return Err(error_callback(lexer));
        }
    }
    char::try_from(value).map_err(|_| error_callback(lexer))
}

fn invalid_callback(
    lexer: &mut logos::Lexer<StringChar>,
) -> Result<char, LexerError> {
    Err(error_callback(lexer))
}

fn error_callback(lexer: &mut logos::Lexer<StringChar>) -> LexerError {
    let contents_start = lexer.extras;
    let span = SrcSpan::from_byte_range(
        (lexer.span().start + contents_start)
            ..(lexer.span().end + contents_start),
    );
    let text = Rc::from(lexer.slice());
    LexerError::InvalidStringEscape(span, text)
}

//===========================================================================//

/// Unescapes a string literael, or returns an error if an invalid escape
/// sequence is found.
///
/// `contents` should be the raw source code contents of the string literal,
/// not including the enclosing quotes.  `contents_start` should give the file
/// byte offset of the start of `contents`.
pub(super) fn unescape_string_literal(
    contents: &str,
    contents_start: usize,
) -> Result<Rc<str>, LexerError> {
    let mut lexer = StringChar::lexer_with_extras(contents, contents_start);
    let mut string = String::new();
    while let Some(result) = lexer.next() {
        match result? {
            StringChar::EscapedChar(chr) => string.push(chr),
            StringChar::LiteralChars => string.push_str(lexer.slice()),
        }
    }
    Ok(Rc::from(string))
}

//===========================================================================//

pub(super) fn hex_digit_value(chr: char) -> Option<u8> {
    match chr {
        'A'..='F' => Some(chr as u8 - b'A' + 10),
        'a'..='f' => Some(chr as u8 - b'a' + 10),
        '0'..='9' => Some(chr as u8 - b'0'),
        _ => None,
    }
}

//===========================================================================//
