use super::binary::{BinaryIo, Decoder, Encoder};
use crate::addr::Offset;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// A symbol defined within an object file.
#[derive(Debug, Eq, PartialEq)]
pub struct ObjSymbol {
    /// The fully qualified name of the symbol.
    pub name: Rc<str>,
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
        let exported = bool::read_from(decoder)?;
        let offset = Offset::read_from(decoder)?;
        Ok(ObjSymbol { name, exported, offset })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.name.write_to(encoder)?;
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
    use crate::obj::assert_round_trips;
    use std::rc::Rc;

    #[test]
    fn round_trips() {
        assert_round_trips(ObjSymbol {
            name: Rc::from(""),
            exported: false,
            offset: Offset::ZERO,
        });
        assert_round_trips(ObjSymbol {
            name: Rc::from("foobar"),
            exported: true,
            offset: Offset::from(1000u32),
        });
    }
}

//===========================================================================//
