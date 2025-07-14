use std::fmt;

//===========================================================================//

/// A partial result from a stream of parser output values.
#[derive(Debug, Eq, PartialEq)]
pub enum StreamResult<T, E> {
    /// More input is needed before another value can be yielded.
    NeedMoreInput,
    /// The next value in the output stream.
    Yield(T),
    /// No more values will be yielded.
    NoMoreOutput,
    /// An error was encountered.
    Error(E),
}

//===========================================================================//

/// A location within a source code file.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SrcLoc {
    /// The line number within the file.  The file starts on line 1.
    pub line: u32,
    /// The column number within the line.  Each line starts at column 0.
    pub column: usize,
}

impl SrcLoc {
    /// Constructs the location for the start of a source code file.
    pub fn start() -> SrcLoc {
        SrcLoc { line: 1, column: 0 }
    }

    /// Advances this location to the start of the next line.  Calling this
    /// increments `self.line`, and sets `self.column` to zero.
    pub fn newline(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

impl fmt::Display for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

//===========================================================================//
