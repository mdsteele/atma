use super::binary::{BinaryIo, Decoder, Encoder};
use super::expr::ObjExpr;
use crate::addr::Offset;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::io;
use std::range::RangeInclusive;

//===========================================================================//

const TAG_FILL: u8 = 0xff;

const TAG_A16R8: u8 = 0x80;
const TAG_A16R16LE: u8 = 0x81;
const TAG_A16RLINK: u8 = 0x82;

const TAG_S8: u8 = 0x00;
const TAG_S16BE: u8 = 0x01;
const TAG_S16LE: u8 = 0x02;
const TAG_S24BE: u8 = 0x03;
const TAG_S24LE: u8 = 0x04;
const TAG_U8: u8 = 0x05;
const TAG_U16BE: u8 = 0x06;
const TAG_U16LE: u8 = 0x07;
const TAG_U24BE: u8 = 0x08;
const TAG_U24LE: u8 = 0x09;

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
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let offset = Offset::read_from(decoder)?;
        let data = ObjPatchData::read_from(decoder)?;
        Ok(ObjPatch { offset, data })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.offset.write_to(encoder)?;
        self.data.write_to(encoder)?;
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
    /// Patch in a relative address delta of the given type, calculated from
    /// the difference between the given expressions.
    Relative(ObjPatchRelType, ObjExpr, ObjExpr),
}

impl ObjPatchData {
    pub(crate) fn num_bytes(&self) -> usize {
        match self {
            ObjPatchData::Fill(size) => *size,
            ObjPatchData::Integer(int_type, _) => int_type.num_bytes(),
            ObjPatchData::Relative(rel_type, _, _) => rel_type.num_bytes(),
        }
    }
}

impl BinaryIo for ObjPatchData {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_FILL => Ok(ObjPatchData::Fill(usize::read_from(decoder)?)),
            byte if byte >= 0x80 => {
                let rel_type = ObjPatchRelType::decode_from_byte(byte)?;
                let lhs = ObjExpr::read_from(decoder)?;
                let rhs = ObjExpr::read_from(decoder)?;
                Ok(ObjPatchData::Relative(rel_type, lhs, rhs))
            }
            byte => {
                let int_type = ObjPatchIntType::decode_from_byte(byte)?;
                let expr = ObjExpr::read_from(decoder)?;
                Ok(ObjPatchData::Integer(int_type, expr))
            }
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        match self {
            ObjPatchData::Fill(size) => {
                TAG_FILL.write_to(encoder)?;
                size.write_to(encoder)
            }
            ObjPatchData::Integer(int_type, expr) => {
                int_type.encode_to_byte().write_to(encoder)?;
                expr.write_to(encoder)
            }
            ObjPatchData::Relative(rel_type, lhs, rhs) => {
                rel_type.encode_to_byte().write_to(encoder)?;
                lhs.write_to(encoder)?;
                rhs.write_to(encoder)
            }
        }
    }
}

//===========================================================================//

/// An integer size and format that can be written as a patch to an object
/// file.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ObjPatchIntType {
    /// Patch a single data byte with an signed 8-bit integer.
    S8,
    /// Patch a single data byte with a big-endian signed 16-bit integer.
    S16be,
    /// Patch a single data byte with a little-endian signed 16-bit integer.
    S16le,
    /// Patch a single data byte with a big-endian signed 24-bit integer.
    S24be,
    /// Patch a single data byte with a little-endian signed 24-bit integer.
    S24le,
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
    fn num_bytes(self) -> usize {
        match self {
            Self::S8 | Self::U8 => 1,
            Self::S16be | Self::S16le | Self::U16be | Self::U16le => 2,
            Self::S24be | Self::S24le | Self::U24be | Self::U24le => 3,
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

    pub(crate) fn append_value(self, value: i64, out: &mut Vec<u8>) {
        match self {
            Self::S8 | Self::U8 => {
                out.push(value as u8);
            }
            Self::S16be | Self::U16be => {
                out.push((value >> 8) as u8);
                out.push(value as u8);
            }
            Self::S16le | Self::U16le => {
                out.push(value as u8);
                out.push((value >> 8) as u8);
            }
            Self::S24be | Self::U24be => {
                out.push((value >> 16) as u8);
                out.push((value >> 8) as u8);
                out.push(value as u8);
            }
            Self::S24le | Self::U24le => {
                out.push(value as u8);
                out.push((value >> 8) as u8);
                out.push((value >> 16) as u8);
            }
        }
    }

    pub(crate) fn write_value_at(
        self,
        value: i64,
        offset: usize,
        data: &mut [u8],
    ) {
        debug_assert!(offset + self.num_bytes() <= data.len());
        match self {
            Self::S8 | Self::U8 => {
                data[offset] = value as u8;
            }
            Self::S16be | Self::U16be => {
                data[offset] = (value >> 8) as u8;
                data[offset + 1] = value as u8;
            }
            Self::S16le | Self::U16le => {
                data[offset] = value as u8;
                data[offset + 1] = (value >> 8) as u8;
            }
            Self::S24be | Self::U24be => {
                data[offset] = (value >> 16) as u8;
                data[offset + 1] = (value >> 8) as u8;
                data[offset + 2] = value as u8;
            }
            Self::S24le | Self::U24le => {
                data[offset] = value as u8;
                data[offset + 1] = (value >> 8) as u8;
                data[offset + 2] = (value >> 16) as u8;
            }
        }
    }

    fn range(self) -> RangeInclusive<i64> {
        match self {
            Self::S8 => RangeInclusive { start: -0x80, last: 0x7f },
            Self::S16be | Self::S16le => {
                RangeInclusive { start: -0x8000, last: 0x7fff }
            }
            Self::S24be | Self::S24le => {
                RangeInclusive { start: -0x800000, last: 0x7fffff }
            }
            Self::U8 => RangeInclusive { start: 0, last: 0xff },
            Self::U16be | Self::U16le => {
                RangeInclusive { start: 0, last: 0xffff }
            }
            Self::U24be | Self::U24le => {
                RangeInclusive { start: 0, last: 0xffffff }
            }
        }
    }

    fn decode_from_byte(byte: u8) -> io::Result<Self> {
        match byte {
            TAG_S8 => Ok(Self::S8),
            TAG_S16BE => Ok(Self::S16be),
            TAG_S16LE => Ok(Self::S16le),
            TAG_S24BE => Ok(Self::S24be),
            TAG_S24LE => Ok(Self::S24le),
            TAG_U8 => Ok(Self::U8),
            TAG_U16BE => Ok(Self::U16be),
            TAG_U16LE => Ok(Self::U16le),
            TAG_U24BE => Ok(Self::U24be),
            TAG_U24LE => Ok(Self::U24le),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid ObjPatchIntType byte: {}", byte),
            )),
        }
    }

    fn encode_to_byte(self) -> u8 {
        match self {
            Self::S8 => TAG_S8,
            Self::S16be => TAG_S16BE,
            Self::S16le => TAG_S16LE,
            Self::S24be => TAG_S24BE,
            Self::S24le => TAG_S24LE,
            Self::U8 => TAG_U8,
            Self::U16be => TAG_U16BE,
            Self::U16le => TAG_U16LE,
            Self::U24be => TAG_U24BE,
            Self::U24le => TAG_U24LE,
        }
    }
}

impl BinaryIo for ObjPatchIntType {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        Self::decode_from_byte(u8::read_from(decoder)?)
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.encode_to_byte().write_to(encoder)
    }
}

//===========================================================================//

/// An relative address delta size and format that can be written as a patch to
/// an object file.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ObjPatchRelType {
    /// A 8-bit signed relative address within a 16-bit address space.
    Addr16Rel8,
    /// A 16-bit signed little-endian relative address within a 16-bit address
    /// space.
    Addr16Rel16le,
    /// A 16-bit signed little-endian relative address, encoded as a SuperFX
    /// LINK opcode.
    Addr16RelLink,
}

impl ObjPatchRelType {
    fn int_type(self) -> ObjPatchIntType {
        match self {
            Self::Addr16Rel8 => ObjPatchIntType::S8,
            Self::Addr16Rel16le => ObjPatchIntType::S16le,
            Self::Addr16RelLink => ObjPatchIntType::U8,
        }
    }

    fn int_value(self, delta: i64) -> i64 {
        match self {
            Self::Addr16RelLink => 0x90 | delta,
            _ => delta,
        }
    }

    fn num_bytes(self) -> usize {
        self.int_type().num_bytes()
    }

    pub(crate) fn delta_value_in_range(
        self,
        delta: &BigInt,
    ) -> Result<i64, RangeInclusive<i64>> {
        let delta_i64 = match self {
            Self::Addr16Rel8 | Self::Addr16Rel16le | Self::Addr16RelLink => {
                let masked = delta & BigInt::from(0xffff);
                let wrapped = u16::try_from(masked).unwrap() as i16;
                wrapped as i64
            }
        };
        let range = self.range();
        if range.contains(&delta_i64) { Ok(delta_i64) } else { Err(range) }
    }

    pub(crate) fn append_delta(self, delta: i64, out: &mut Vec<u8>) {
        self.int_type().append_value(self.int_value(delta), out);
    }

    pub(crate) fn write_delta_at(
        self,
        delta: i64,
        offset: usize,
        data: &mut [u8],
    ) {
        self.int_type().write_value_at(self.int_value(delta), offset, data);
    }

    fn range(self) -> RangeInclusive<i64> {
        match self {
            Self::Addr16RelLink => RangeInclusive { start: 1, last: 4 },
            _ => self.int_type().range(),
        }
    }

    fn decode_from_byte(byte: u8) -> io::Result<Self> {
        match byte {
            TAG_A16R8 => Ok(Self::Addr16Rel8),
            TAG_A16R16LE => Ok(Self::Addr16Rel16le),
            TAG_A16RLINK => Ok(Self::Addr16RelLink),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid ObjPatchRelType byte: {}", byte),
            )),
        }
    }

    fn encode_to_byte(self) -> u8 {
        match self {
            Self::Addr16Rel8 => TAG_A16R8,
            Self::Addr16Rel16le => TAG_A16R16LE,
            Self::Addr16RelLink => TAG_A16RLINK,
        }
    }
}

impl BinaryIo for ObjPatchRelType {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        Self::decode_from_byte(u8::read_from(decoder)?)
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.encode_to_byte().write_to(encoder)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ObjPatch, ObjPatchData, ObjPatchIntType, ObjPatchRelType};
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
        assert_round_trips(ObjPatchData::Relative(
            ObjPatchRelType::Addr16Rel8,
            ObjExpr::from(ExprValue::Integer(BigInt::from(0x1234))),
            ObjExpr::from(ExprValue::Integer(BigInt::from(0x1256))),
        ));
        assert_round_trips(ObjPatchData::Relative(
            ObjPatchRelType::Addr16Rel16le,
            ObjExpr::from(ExprValue::Integer(BigInt::from(0x5678))),
            ObjExpr::from(ExprValue::Integer(BigInt::from(0x1234))),
        ));
    }

    #[test]
    fn obj_patch_int_type_round_trips() {
        assert_round_trips(ObjPatchIntType::S8);
        assert_round_trips(ObjPatchIntType::S16be);
        assert_round_trips(ObjPatchIntType::S16le);
        assert_round_trips(ObjPatchIntType::S24be);
        assert_round_trips(ObjPatchIntType::S24le);
        assert_round_trips(ObjPatchIntType::U8);
        assert_round_trips(ObjPatchIntType::U16be);
        assert_round_trips(ObjPatchIntType::U16le);
        assert_round_trips(ObjPatchIntType::U24be);
        assert_round_trips(ObjPatchIntType::U24le);
    }

    #[test]
    fn obj_patch_rel_type_round_trips() {
        assert_round_trips(ObjPatchRelType::Addr16Rel8);
        assert_round_trips(ObjPatchRelType::Addr16Rel16le);
        assert_round_trips(ObjPatchRelType::Addr16RelLink);
    }
}

//===========================================================================//
