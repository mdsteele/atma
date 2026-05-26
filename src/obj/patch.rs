use super::binary::BinaryIo;
use super::expr::ObjExpr;
use crate::addr::Offset;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::io;
use std::ops::RangeInclusive;

//===========================================================================//

/// A patch to apply to an object file during linking.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjPatch {
    /// The offset from the start of the chunk to the start of the patch, in
    /// bytes.
    pub offset: Offset,
    /// The patch type and value to apply.
    pub data: ObjPatchData,
}

impl BinaryIo for ObjPatch {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let offset = Offset::read_from(reader)?;
        let data = ObjPatchData::read_from(reader)?;
        Ok(ObjPatch { offset, data })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.offset.write_to(writer)?;
        self.data.write_to(writer)?;
        Ok(())
    }
}

//===========================================================================//

/// The size and format of a given object file patch to apply.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ObjPatchData {
    /// Fills the given number of bytes with the fill byte for this chunk.
    Fill(usize),
    /// Patch in an integer value of the given type with the value of the given
    /// expression.
    Integer(ObjPatchIntType, ObjExpr),
}

impl ObjPatchData {
    pub(crate) fn num_bytes(&self) -> usize {
        match self {
            ObjPatchData::Fill(size) => *size,
            ObjPatchData::Integer(int_type, _) => int_type.num_bytes(),
        }
    }
}

impl BinaryIo for ObjPatchData {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            0xff => Ok(ObjPatchData::Fill(usize::read_from(reader)?)),
            byte => {
                let int_type = ObjPatchIntType::decode_from_byte(byte)?;
                let expr = ObjExpr::read_from(reader)?;
                Ok(ObjPatchData::Integer(int_type, expr))
            }
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            ObjPatchData::Fill(size) => {
                0xffu8.write_to(writer)?;
                size.write_to(writer)
            }
            ObjPatchData::Integer(int_type, expr) => {
                int_type.write_to(writer)?;
                expr.write_to(writer)
            }
        }
    }
}

//===========================================================================//

/// An integer size and format that can be written as a patch to an object
/// file.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ObjPatchIntType {
    /// Patch a single data byte with an unsigned 8-bit integer.
    U8,
    /// Patch two data bytes with a big-endian unsigned 16-bit integer.
    U16be,
    /// Patch two data bytes with a little-endian unsigned 16-bit integer.
    U16le,
    /// Patch three data bytes with a big-endian unsigned 24-bit integer.
    U24be,
    /// Patch three data bytes with a little-endian unsigned 24-bit integer.
    U24le,
}

impl ObjPatchIntType {
    pub(crate) fn num_bytes(self) -> usize {
        match self {
            ObjPatchIntType::U8 => 1,
            ObjPatchIntType::U16be => 2,
            ObjPatchIntType::U16le => 2,
            ObjPatchIntType::U24be => 3,
            ObjPatchIntType::U24le => 3,
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
            ObjPatchIntType::U8 => 0..=0xff,
            ObjPatchIntType::U16be | ObjPatchIntType::U16le => 0..=0xffff,
            ObjPatchIntType::U24be | ObjPatchIntType::U24le => 0..=0xffffff,
        }
    }

    fn decode_from_byte(byte: u8) -> io::Result<ObjPatchIntType> {
        match byte {
            0 => Ok(ObjPatchIntType::U8),
            1 => Ok(ObjPatchIntType::U16be),
            2 => Ok(ObjPatchIntType::U16le),
            3 => Ok(ObjPatchIntType::U24be),
            4 => Ok(ObjPatchIntType::U24le),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid ObjPatchIntType byte: {}", byte),
            )),
        }
    }
}

impl BinaryIo for ObjPatchIntType {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        ObjPatchIntType::decode_from_byte(u8::read_from(reader)?)
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            ObjPatchIntType::U8 => 0u8.write_to(writer),
            ObjPatchIntType::U16be => 1u8.write_to(writer),
            ObjPatchIntType::U16le => 2u8.write_to(writer),
            ObjPatchIntType::U24be => 3u8.write_to(writer),
            ObjPatchIntType::U24le => 4u8.write_to(writer),
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ObjPatch, ObjPatchData, ObjPatchIntType};
    use crate::addr::Offset;
    use crate::expr::ExprValue;
    use crate::obj::ObjExpr;
    use crate::obj::assert_round_trips;
    use num_bigint::BigInt;

    #[test]
    fn obj_patch_round_trips() {
        assert_round_trips(ObjPatch {
            offset: Offset::from(17u32),
            data: ObjPatchData::Integer(
                ObjPatchIntType::U8,
                ObjExpr::from(ExprValue::Integer(BigInt::from(0x12))),
            ),
        });
    }

    #[test]
    fn obj_patch_data_round_trips() {
        assert_round_trips(ObjPatchData::Fill(0));
        assert_round_trips(ObjPatchData::Fill(1));
        assert_round_trips(ObjPatchData::Fill(0x10000));
        assert_round_trips(ObjPatchData::Integer(
            ObjPatchIntType::U8,
            ObjExpr::from(ExprValue::Integer(BigInt::from(0x12))),
        ));
        assert_round_trips(ObjPatchData::Integer(
            ObjPatchIntType::U16le,
            ObjExpr::from(ExprValue::Integer(BigInt::from(0x1234))),
        ));
        assert_round_trips(ObjPatchData::Integer(
            ObjPatchIntType::U24le,
            ObjExpr::from(ExprValue::Integer(BigInt::from(0x123456))),
        ));
    }

    #[test]
    fn obj_patch_int_type_round_trips() {
        assert_round_trips(ObjPatchIntType::U8);
        assert_round_trips(ObjPatchIntType::U16be);
        assert_round_trips(ObjPatchIntType::U16le);
        assert_round_trips(ObjPatchIntType::U24be);
        assert_round_trips(ObjPatchIntType::U24le);
    }
}

//===========================================================================//
