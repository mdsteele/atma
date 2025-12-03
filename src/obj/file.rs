use super::binary::BinaryIo;
use super::chunk::ObjectChunk;
use std::io;

//===========================================================================//

/// Represents an object file assembled from a source file.
pub struct ObjectFile {
    /// The section chunks to be linked.
    pub chunks: Vec<ObjectChunk>,
}

impl BinaryIo for ObjectFile {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let chunks = Vec::<ObjectChunk>::read_from(reader)?;
        Ok(ObjectFile { chunks })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.chunks.write_to(writer)?;
        Ok(())
    }
}

//===========================================================================//
