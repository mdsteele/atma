use super::binary::BinaryIo;
use super::expr::ObjExpr;
use crate::addr::Offset;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::io;
use std::ops::RangeInclusive;

//===========================================================================//

/// A patch to apply to an object file during linking.
#[derive(Clone)]
pub struct ObjPatch {
    /// The offset from the start of the chunk to the start of the patch, in
    /// bytes.
    pub offset: Offset,
    /// The size and format of the patch to apply.
    pub kind: PatchKind,
    /// An expression that will evaluate at link time to the value to patch in.
    pub expr: ObjExpr,
}

impl BinaryIo for ObjPatch {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let offset = Offset::read_from(reader)?;
        let kind = PatchKind::read_from(reader)?;
        let expr = ObjExpr::read_from(reader)?;
        Ok(ObjPatch { offset, kind, expr })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.offset.write_to(writer)?;
        self.kind.write_to(writer)?;
        self.expr.write_to(writer)?;
        Ok(())
    }
}

//===========================================================================//

/// The size and format of a given object file patch to apply.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum PatchKind {
    /// Patch a single data byte with an unsigned 8-bit integer.
    U8,
    /// Patch two data bytes with a little-endian unsigned 16-bit integer.
    U16le,
    /// Patch three data bytes with a little-endian unsigned 24-bit integer.
    U24le,
}

impl PatchKind {
    pub(crate) fn directive(self) -> &'static str {
        match self {
            PatchKind::U8 => ".U8",
            PatchKind::U16le => ".U16LE",
            PatchKind::U24le => ".U24LE",
        }
    }

    pub(crate) fn num_bytes(self) -> usize {
        match self {
            PatchKind::U8 => 1,
            PatchKind::U16le => 2,
            PatchKind::U24le => 3,
        }
    }

    pub(crate) fn value_in_range(
        self,
        bigint: &BigInt,
    ) -> Result<i64, RangeInclusive<i64>> {
        let range = self.range();
        match bigint.to_i64() {
            None => Err(range),
            Some(value) => {
                if range.contains(&value) {
                    Ok(value)
                } else {
                    Err(range)
                }
            }
        }
    }

    fn range(self) -> RangeInclusive<i64> {
        match self {
            PatchKind::U8 => 0..=0xff,
            PatchKind::U16le => 0..=0xffff,
            PatchKind::U24le => 0..=0xffffff,
        }
    }
}

impl BinaryIo for PatchKind {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            0 => Ok(PatchKind::U8),
            1 => Ok(PatchKind::U16le),
            2 => Ok(PatchKind::U24le),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid PatchKind byte: {}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        let byte: u8 = match self {
            PatchKind::U8 => 0,
            PatchKind::U16le => 1,
            PatchKind::U24le => 2,
        };
        byte.write_to(writer)
    }
}

//===========================================================================//
