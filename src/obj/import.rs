use super::binary::{BinaryIo, Decoder, Encoder};
use super::context::ObjSrcLoc;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// An external symbol imported by an object file.
pub struct ObjImport {
    /// The fully qualified name of the imported symbol.
    pub full_name: Rc<str>,
    /// The assembly source code location where the symbol was imported.
    pub loc: ObjSrcLoc,
}

impl BinaryIo for ObjImport {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let full_name = Rc::<str>::read_from(decoder)?;
        let loc = ObjSrcLoc::read_from(decoder)?;
        Ok(ObjImport { full_name, loc })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.full_name.write_to(encoder)?;
        self.loc.write_to(encoder)?;
        Ok(())
    }
}

//===========================================================================//
