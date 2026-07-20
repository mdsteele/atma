//! Types for error reporting.

mod cache;
mod errs;
mod span;

pub use cache::{SrcCache, SrcCacheError, StrSrcCache};
pub use errs::{Errs, ErrsIntoIter};
pub use span::{SrcLoc, SrcSpan};

//===========================================================================//

/// An error that refers to one or more locations in source code files.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceError {
    /// The primary source code location where the error occurred.
    pub loc: SrcLoc,
    /// The error message to report to the user.
    pub message: String,
    /// Any additional label annotations for this error.
    pub labels: Vec<SourceErrorLabel>,
    /// Any additional note messages that may help the user to understand how
    /// to fix the error.
    pub notes: Vec<String>,
}

impl SourceError {
    /// Constructs a parse error with the given location and message, and other
    /// fields initially empty.
    pub fn new(loc: SrcLoc, message: impl ToString) -> SourceError {
        SourceError {
            loc,
            message: message.to_string(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Adds a label to the error using the error's primary location.
    pub fn with_primary_label(self, message: impl ToString) -> SourceError {
        let loc = self.loc.clone();
        self.with_label(loc, message)
    }

    /// Adds an additional label to the error.
    pub fn with_label(
        mut self,
        loc: SrcLoc,
        message: impl ToString,
    ) -> SourceError {
        self.labels
            .push(SourceErrorLabel { loc, message: message.to_string() });
        self
    }

    /// Adds additional context to the error.
    pub fn with_context(self, context: &impl SourceContext) -> SourceError {
        context.annotate(self)
    }

    /// Adds an additional note to the error.
    pub fn with_note(mut self, message: impl ToString) -> SourceError {
        self.notes.push(message.to_string());
        self
    }
}

//===========================================================================//

/// An additional label annotation for a [`SourceError`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SourceErrorLabel {
    /// The source code location to which the label applies.
    pub loc: SrcLoc,
    /// The message to attach to the label.
    pub message: String,
}

//===========================================================================//

/// A specialized `Result` type for processing source code files.
pub type SourceResult<T> = Result<T, Errs<SourceError>>;

//===========================================================================//

/// Additional context for a [`SourceError`].
pub trait SourceContext {
    /// Annotates the given source error with the context in `self`.
    ///
    /// Typically callers should use [`SourceError::with_context`] instead of
    /// calling this directly.
    fn annotate(&self, error: SourceError) -> SourceError;
}

//===========================================================================//
