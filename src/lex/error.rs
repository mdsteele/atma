use super::token::Token;
use crate::error::{SourceError, SrcLoc, SrcSpan};
use std::rc::Rc;

//===========================================================================//

/// An error encountered while tokenizing a source code file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LexerError {
    /// Found a backslash escaping another backslash.
    BackslashBeforeBackslash(SrcSpan, SrcSpan),
    /// Found a stray backslash at the end of the source file.
    BackslashBeforeEof(SrcSpan),
    /// Found a backslash escaping a token that's ineligable for escaping.
    BackslashBeforeToken(SrcSpan, Token),
    /// Found an invalid escape sequence in a string literal.
    InvalidStringEscape(SrcSpan, Rc<str>),
    /// Found a character sequence that couldn't be recognized as a valid
    /// token.
    UnrecognizedToken(SrcSpan, Rc<str>),
}

impl LexerError {
    /// Converts the error into a `SourceError`, using the given path for the
    /// source file being lexed.
    pub fn to_source_error(self, path: &Rc<str>) -> SourceError {
        match self {
            LexerError::BackslashBeforeBackslash(first_span, _second_span) => {
                let message = "unexpected backslash before backslash";
                SourceError::new(SrcLoc::new(path, first_span), message)
                    .with_primary_label("")
            }
            LexerError::BackslashBeforeEof(backslash_span) => {
                let message = "unexpected backslash before EOF";
                SourceError::new(SrcLoc::new(path, backslash_span), message)
                    .with_primary_label("")
            }
            LexerError::BackslashBeforeToken(backslash_span, token) => {
                let message = format!(
                    "unexpected backslash before {}",
                    token.value.name()
                );
                SourceError::new(SrcLoc::new(path, backslash_span), message)
                    .with_primary_label("")
            }
            LexerError::InvalidStringEscape(escape_span, escape_string) => {
                let message =
                    format!("invalid string escape: `{escape_string}`");
                SourceError::new(SrcLoc::new(path, escape_span), message)
                    .with_primary_label("")
            }
            LexerError::UnrecognizedToken(span, text) => {
                let message =
                    format!("unrecognized token: '{}'", text.escape_debug());
                SourceError::new(SrcLoc::new(path, span), message)
                    .with_primary_label("")
            }
        }
    }
}

impl From<LogosLexerError> for LexerError {
    fn from(error: LogosLexerError) -> Self {
        error.0
    }
}

//===========================================================================//

/// Internal-only wrapper around `LexerError` that can have a stub `Default`
/// implementation (which `logos` needs), so that `LexerError` doesn't have to.
///
/// If `logos` ever stops needing a `Default` implementation for errors, we can
/// get rid of this type and just use `LexerError` directly.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct LogosLexerError(pub LexerError);

impl Default for LogosLexerError {
    fn default() -> Self {
        Self(LexerError::UnrecognizedToken(
            SrcSpan::from_byte_range(0..0),
            Rc::from(""),
        ))
    }
}

impl From<LexerError> for LogosLexerError {
    fn from(error: LexerError) -> Self {
        Self(error)
    }
}

//===========================================================================//
