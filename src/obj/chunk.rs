use super::align::Align32;
use super::binary::BinaryIo;
use super::symbol::ObjectSymbol;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// Represents one data chunk of an object file.
#[derive(Clone)]
pub struct ObjectChunk {
    /// The name of the linker section to which this chunk belongs.
    pub section_name: Rc<str>,
    /// Static data (before rewrite rules are applied) at the start of this
    /// chunk.
    pub data: Rc<[u8]>,
    /// The size of the chunk, in bytes.  This may be greater than `data.len()`
    /// if the chunk requires additional padding after the data.  This should
    /// not be less than `data.len()`, or else the linker will return an error
    /// when trying to place this chunk.
    pub size: u32,
    /// The required alignment for this chunk's data, within its address space.
    pub align: Align32,
    /// If set, then this entire chunk (data + padding) must not cross any
    /// alignment boundary of this size within its address space.
    pub within: Option<Align32>,
    /// Relative symbols defined in this chunk.
    pub symbols: Rc<[ObjectSymbol]>,
}

impl BinaryIo for ObjectChunk {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let section_name = Rc::<str>::read_from(reader)?;
        let data = Rc::<[u8]>::read_from(reader)?;
        let size = u32::read_from(reader)?;
        let align = Align32::read_from(reader)?;
        let within = Option::<Align32>::read_from(reader)?;
        let symbols = Rc::<[ObjectSymbol]>::read_from(reader)?;
        Ok(ObjectChunk { section_name, data, size, align, within, symbols })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.section_name.write_to(writer)?;
        self.data.write_to(writer)?;
        self.size.write_to(writer)?;
        self.align.write_to(writer)?;
        self.within.write_to(writer)?;
        self.symbols.write_to(writer)?;
        Ok(())
    }
}

//===========================================================================//
