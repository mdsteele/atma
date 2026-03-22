use super::lex::LexerError;
use crate::error::{SourceError, SrcSpan, ToSourceError};
use std::rc::Rc;

//===========================================================================//

/// A specialized `Result` type for parsing operations.
pub type ParseResult<T> = Result<T, Vec<ParseError>>;

//===========================================================================//

/// An error encountered while parsing a source code file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    /// Failed to tokenize the source code.
    Lex(LexerError),
    /// Expected a different token next.
    ExpectedToken(SrcSpan, Rc<str>),
}

impl ToSourceError for ParseError {
    fn to_source_error(self) -> SourceError {
        match self {
            ParseError::Lex(error) => error.to_source_error(),
            ParseError::ExpectedToken(span, expectation) => {
                SourceError::new(span, expectation.to_string())
            }
        }
    }
}

//===========================================================================//
