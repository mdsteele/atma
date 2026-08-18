use super::binary::{BinaryIo, Decoder, Encoder};
use super::context::ObjSrcLoc;
use crate::addr::Offset;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// A symbol defined within an object file.
#[derive(Debug, Eq, PartialEq)]
pub struct ObjSymbol {
    /// The fully qualified name of the symbol.
    pub name: Rc<str>,
    /// The source code location where the symbol is declared.
    pub loc: ObjSrcLoc,
    /// True if this symbol may be imported by other object files during
    /// linking, false if it is local to this object file.
    pub exported: bool,
    /// The offset from the start of the chunk, in bytes.
    pub offset: Offset,
}

impl BinaryIo for ObjSymbol {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let name = Rc::<str>::read_from(decoder)?;
        let loc = ObjSrcLoc::read_from(decoder)?;
        let exported = bool::read_from(decoder)?;
        let offset = Offset::read_from(decoder)?;
        Ok(ObjSymbol { name, loc, exported, offset })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.name.write_to(encoder)?;
        self.loc.write_to(encoder)?;
        self.exported.write_to(encoder)?;
        self.offset.write_to(encoder)?;
        Ok(())
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::ObjSymbol;
    use crate::addr::Offset;
    use crate::error::SrcSpan;
    use crate::obj::{
        ObjSrcContext, ObjSrcLoc, ObjSrcParent, assert_round_trips,
    };
    use std::rc::Rc;

    #[test]
    fn round_trips() {
        assert_round_trips(ObjSymbol {
            name: Rc::from("foobar"),
            loc: ObjSrcLoc {
                span: SrcSpan::from_byte_range(5..15),
                context: Rc::new(ObjSrcContext {
                    path: Rc::from("foo/bar.asm"),
                    parent: ObjSrcParent::Root,
                }),
            },
            exported: true,
            offset: Offset::from(1000u32),
        });
    }
}

//===========================================================================//
