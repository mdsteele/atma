//! Types for error reporting.

mod cache;
mod errs;
mod span;

pub use cache::{SrcCache, SrcCacheError, StrSrcCache};
pub use errs::{Errs, ErrsIntoIter};
pub use span::SrcSpan;

//===========================================================================//

/// An error that refers to one or more locations in source code files.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceError {
    /// The primary location in the file where the error occurred.
    pub span: SrcSpan,
    /// The error message to report to the user.
    pub message: String,
    /// Any additional label annotations for this error.
    pub labels: Vec<SourceErrorLabel>,
}

impl SourceError {
    /// Constructs a parse error with the given span and message, and other
    /// fields initially empty.
    pub fn new(span: SrcSpan, message: String) -> SourceError {
        SourceError { span, message, labels: Vec::new() }
    }

    /// Adds an additional label to the error.
    pub fn with_label(
        mut self,
        span: SrcSpan,
        message: String,
    ) -> SourceError {
        self.labels.push(SourceErrorLabel { span, message });
        self
    }

    /// Converts a list of errors into a list of `SourceError`s.
    pub fn from_errors<E: ToSourceError>(errors: Vec<E>) -> Errs<SourceError> {
        errors.into_iter().map(E::to_source_error).collect()
    }
}

//===========================================================================//

/// An additional label annotation for a [`SourceError`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceErrorLabel {
    /// The location in the file to which the label applies.
    pub span: SrcSpan,
    /// The message to attach to the label.
    pub message: String,
}

//===========================================================================//

/// A specialized `Result` type for processing source code files.
pub type SourceResult<T> = Result<T, Errs<SourceError>>;

//===========================================================================//

/// A trait for error types that can be converted into `SourceError`s.
pub trait ToSourceError {
    /// Converts the error into a `SourceError`.
    fn to_source_error(self) -> SourceError;
}

//===========================================================================//
