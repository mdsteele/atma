use std::ops::Range;
use std::rc::Rc;

//===========================================================================//

/// A span of byte offsets within a particular source code file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SrcLoc {
    /// The path for the source code file.
    pub path: Rc<str>,
    /// The span of byte offsets within the file.
    pub span: SrcSpan,
}

impl SrcLoc {
    /// Constructs a source location within the specified source code file.
    pub fn new(path: &Rc<str>, span: SrcSpan) -> SrcLoc {
        SrcLoc { path: path.clone(), span }
    }
}

//===========================================================================//

/// A span of byte offsets within source code text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SrcSpan {
    start: usize,
    end: usize,
}

impl SrcSpan {
    /// A [SrcSpan] for code that is built in to ATMA and doesn't actually
    /// exist in any source code file.
    pub const BUILTIN: SrcSpan = SrcSpan { start: 0, end: 0 };

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
