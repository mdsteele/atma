use super::binary::BinaryIo;
use super::chunk::ObjChunk;
use std::io;

//===========================================================================//

/// Represents an object file assembled from a source file.
pub struct ObjFile {
    /// The section chunks to be linked.
    pub chunks: Vec<ObjChunk>,
}

impl BinaryIo for ObjFile {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let chunks = Vec::<ObjChunk>::read_from(reader)?;
        Ok(ObjFile { chunks })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.chunks.write_to(writer)?;
        Ok(())
    }
}

//===========================================================================//
