use super::assert::ObjAssert;
use super::binary::{BinaryIo, Decoder, Encoder};
use super::chunk::ObjChunk;
use super::expr::ObjExpr;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// Represents an object file assembled from a source file.
pub struct ObjFile {
    /// The section chunks to be linked.
    pub chunks: Vec<ObjChunk>,
    /// The fully qualified names of the symbols imported by this object file.
    pub imports: Vec<Rc<str>>,
    /// Local non-static variables declared in this object file, which are to
    /// evaluated (in order) after all symbols have been resolved, and can then
    /// be used by assertion and patch expressions (or by later variables).
    pub variables: Vec<ObjExpr>,
    /// Any assertions that must be met at link time.
    pub asserts: Vec<ObjAssert>,
}

impl BinaryIo for ObjFile {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let chunks = Vec::<ObjChunk>::read_from(decoder)?;
        let imports = Vec::<Rc<str>>::read_from(decoder)?;
        let variables = Vec::<ObjExpr>::read_from(decoder)?;
        let asserts = Vec::<ObjAssert>::read_from(decoder)?;
        Ok(ObjFile { chunks, imports, variables, asserts })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.chunks.write_to(encoder)?;
        self.imports.write_to(encoder)?;
        self.variables.write_to(encoder)?;
        self.asserts.write_to(encoder)?;
        Ok(())
    }
}

//===========================================================================//
