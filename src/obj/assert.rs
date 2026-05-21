use super::binary::BinaryIo;
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
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let condition = ObjExpr::read_from(reader)?;
        let message = Option::<ObjExpr>::read_from(reader)?;
        Ok(ObjAssert { condition, message })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.condition.write_to(writer)?;
        self.message.write_to(writer)?;
        Ok(())
    }
}

//===========================================================================//
