use super::lex::LexerError;
use crate::error::{Errs, SourceError, SrcLoc, SrcSpan};
use std::rc::Rc;

//===========================================================================//

/// A specialized `Result` type for parsing operations.
pub type ParseResult<T> = Result<T, Errs<ParseError>>;

//===========================================================================//

/// An error encountered while parsing a source code file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    /// Failed to tokenize the source code.
    Lex(LexerError),
    /// Expected a different token next.
    ExpectedToken(SrcSpan, Rc<str>),
}

impl ParseError {
    /// Converts the error into a `SourceError`, using the given path for the
    /// source file being parsed.
    pub fn to_source_error(self, path: &Rc<str>) -> SourceError {
        match self {
            ParseError::Lex(error) => error.to_source_error(path),
            ParseError::ExpectedToken(span, expectation) => {
                SourceError::new(SrcLoc::new(path, span), &expectation)
                    .with_primary_label("")
            }
        }
    }
}

//===========================================================================//
