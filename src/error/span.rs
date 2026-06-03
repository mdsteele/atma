use std::ops::Range;

//===========================================================================//

/// A span of byte offsets within a source code file.
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
