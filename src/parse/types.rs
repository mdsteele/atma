use std::ops::Range;

//===========================================================================//

/// A span of byte offsets within a source code file.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SrcSpan {
    start: usize,
    end: usize,
}

impl SrcSpan {
    /// Constructs a span from the given the byte range.
    pub fn from_byte_range(range: Range<usize>) -> SrcSpan {
        assert!(range.start <= range.end);
        SrcSpan { start: range.start, end: range.end }
    }

    /// Returns the byte range represented by this span.
    pub fn byte_range(&self) -> Range<usize> {
        self.start..self.end
    }

    /// Merges two spans, returning the smallest span that covers both.
    pub fn merged_with(&self, other: SrcSpan) -> SrcSpan {
        SrcSpan {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub(crate) fn end_span(&self) -> SrcSpan {
        SrcSpan { start: self.end, end: self.end }
    }
}

//===========================================================================//

/// A specialized `Result` type for parsing operations.
pub type ParseResult<V> = Result<V, Vec<ParseError>>;

//===========================================================================//

/// An error encountered while parsing a source code file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
    /// The primary location in the file where the error occurred.
    pub span: SrcSpan,
    /// The error message to report to the user.
    pub message: String,
    /// Any additional label annotations for this error.
    pub labels: Vec<ParseErrorLabel>,
}

impl ParseError {
    /// Constructs a parse error with the given span and message, and other
    /// fields initially empty.
    pub fn new(span: SrcSpan, message: String) -> ParseError {
        ParseError { span, message, labels: Vec::new() }
    }

    /// Adds an additional label to the error.
    pub fn with_label(mut self, span: SrcSpan, message: String) -> ParseError {
        self.labels.push(ParseErrorLabel { span, message });
        self
    }
}

//===========================================================================//

/// An additional label annotation for a [`ParseError`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseErrorLabel {
    /// The location in the file to which the label applies.
    pub span: SrcSpan,
    /// The message to attach to the label.
    pub message: String,
}

//===========================================================================//
