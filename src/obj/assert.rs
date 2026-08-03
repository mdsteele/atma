use super::binary::{BinaryIo, Decoder, Encoder};
use super::expr::ObjExpr;
use std::io;

//===========================================================================//

/// A boolean assertion that must be true at link time for linking to succeed.
pub struct ObjAssert {
    /// The boolean condition that is expected to be true.
    pub condition: ObjExpr,
    /// An optional error message that should be emitted if the assertion
    /// fails.
    pub message: Option<ObjExpr>,
}

impl BinaryIo for ObjAssert {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let condition = ObjExpr::read_from(decoder)?;
        let message = Option::<ObjExpr>::read_from(decoder)?;
        Ok(ObjAssert { condition, message })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.condition.write_to(encoder)?;
        self.message.write_to(encoder)?;
        Ok(())
    }
}

//===========================================================================//
