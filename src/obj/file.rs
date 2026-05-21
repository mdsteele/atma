use super::assert::ObjAssert;
use super::binary::BinaryIo;
use super::chunk::ObjChunk;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// Represents an object file assembled from a source file.
pub struct ObjFile {
    /// The section chunks to be linked.
    pub chunks: Vec<ObjChunk>,
    /// The fully qualified names of the symbols imported by this object file.
    pub imports: Vec<Rc<str>>,
    /// Any assertions that must be met at link time.
    pub asserts: Vec<ObjAssert>,
}

impl BinaryIo for ObjFile {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let chunks = Vec::<ObjChunk>::read_from(reader)?;
        let imports = Vec::<Rc<str>>::read_from(reader)?;
        let asserts = Vec::<ObjAssert>::read_from(reader)?;
        Ok(ObjFile { chunks, imports, asserts })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.chunks.write_to(writer)?;
        self.imports.write_to(writer)?;
        self.asserts.write_to(writer)?;
        Ok(())
    }
}

//===========================================================================//
