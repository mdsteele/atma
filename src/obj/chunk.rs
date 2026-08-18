use super::binary::{BinaryIo, Decoder, Encoder};
use super::context::ObjSrcLoc;
use super::patch::ObjPatch;
use super::symbol::ObjSymbol;
use crate::addr::{Addr, Align, Size};
use std::io;
use std::rc::Rc;

//===========================================================================//

/// Represents one data chunk of an object file.
pub struct ObjChunk {
    /// The name of the linker section to which this chunk belongs.
    pub section_name: Rc<str>,
    /// The source code location for the expression in the chunk declaration
    /// that evaluated to the section name.
    pub section_name_loc: ObjSrcLoc,
    /// Static data (before patches are applied) at the start of this chunk.
    pub data: Box<[u8]>,
    /// The size of the chunk, in bytes.  This may be greater than `data.len()`
    /// if the chunk requires additional padding after the data.  This should
    /// not be less than `data.len()`, or else the linker will return an error
    /// when trying to place this chunk.
    pub size: Size,
    /// If set, then the chunk must start at this exact address within its
    /// address space.
    pub start: Option<Addr>,
    /// The required alignment for this chunk's data, within its address space.
    pub align: Align,
    /// If set, then this entire chunk (data + padding) must not cross any
    /// alignment boundary of this size within its address space.
    pub within: Option<Align>,
    /// If set, then any padded portions of the chunk will be filled with this
    /// byte value. Otherwise, they will be filled with this chunk's section's
    /// fill byte.
    pub fill: Option<u8>,
    /// Relative symbols defined in this chunk.
    pub symbols: Box<[ObjSymbol]>,
    /// Patches to apply to this chunk's data when linking.
    pub patches: Box<[ObjPatch]>,
}

impl BinaryIo for ObjChunk {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let section_name = Rc::<str>::read_from(decoder)?;
        let section_name_loc = ObjSrcLoc::read_from(decoder)?;
        let data = Box::<[u8]>::read_from(decoder)?;
        let size = Size::read_from(decoder)?;
        let start = Option::<Addr>::read_from(decoder)?;
        let align = Align::read_from(decoder)?;
        let within = Option::<Align>::read_from(decoder)?;
        let fill = Option::<u8>::read_from(decoder)?;
        let symbols = Box::<[ObjSymbol]>::read_from(decoder)?;
        let patches = Box::<[ObjPatch]>::read_from(decoder)?;
        Ok(ObjChunk {
            section_name,
            section_name_loc,
            data,
            size,
            start,
            align,
            within,
            fill,
            symbols,
            patches,
        })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.section_name.write_to(encoder)?;
        self.section_name_loc.write_to(encoder)?;
        self.data.write_to(encoder)?;
        self.size.write_to(encoder)?;
        self.start.write_to(encoder)?;
        self.align.write_to(encoder)?;
        self.within.write_to(encoder)?;
        self.fill.write_to(encoder)?;
        self.symbols.write_to(encoder)?;
        self.patches.write_to(encoder)?;
        Ok(())
    }
}

//===========================================================================//
