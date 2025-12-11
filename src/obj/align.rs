use super::binary::BinaryIo;
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::ToPrimitive;
use std::fmt;
use std::io;
use std::num::NonZero;

//===========================================================================//

const UNALIGNED_32: Align32 = Align32(NonZero::new(1).unwrap());

//===========================================================================//

/// Represents a memory alignment for a 32-bit address.
///
/// Essentially, an `Align32` is an unsigned 32-bit integer that is guaranteed
/// to be a power of two.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Align32(NonZero<u32>);

impl Align32 {
    fn decode_from_u8(byte: u8) -> io::Result<Align32> {
        if (1..=32).contains(&byte) {
            Ok(Align32(NonZero::new(1u32 << (byte - 1)).unwrap()))
        } else {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid Align32 byte: {:#02x}", byte),
            ))
        }
    }

    fn encode_as_u8(self) -> u8 {
        (self.0.ilog2() + 1) as u8
    }
}

impl Default for Align32 {
    fn default() -> Align32 {
        UNALIGNED_32
    }
}

impl BinaryIo for Align32 {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        Align32::decode_from_u8(u8::read_from(reader)?)
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.encode_as_u8().write_to(writer)
    }

    fn read_option_from<R: io::BufRead>(
        reader: &mut R,
    ) -> io::Result<Option<Self>> {
        let byte = u8::read_from(reader)?;
        if byte == 0 {
            Ok(None)
        } else {
            Align32::decode_from_u8(byte).map(Some)
        }
    }

    fn write_option_to<W: io::Write>(
        option: &Option<Align32>,
        writer: &mut W,
    ) -> io::Result<()> {
        option.map(Align32::encode_as_u8).unwrap_or(0u8).write_to(writer)
    }
}

impl fmt::Debug for Align32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Align32({:?})", self.0)
    }
}

impl fmt::Display for Align32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Binary for Align32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Octal for Align32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::LowerHex for Align32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::UpperHex for Align32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl From<Align32> for u32 {
    fn from(align: Align32) -> u32 {
        align.0.get()
    }
}

impl TryFrom<u32> for Align32 {
    type Error = AlignTryFromError;

    fn try_from(value: u32) -> Result<Align32, AlignTryFromError> {
        if let Some(nonzero) = NonZero::new(value)
            && nonzero.is_power_of_two()
        {
            Ok(Align32(nonzero))
        } else {
            Err(AlignTryFromError::NotAPowerOfTwo)
        }
    }
}

impl TryFrom<&BigUint> for Align32 {
    type Error = AlignTryFromError;

    fn try_from(value: &BigUint) -> Result<Align32, AlignTryFromError> {
        match value.to_u32() {
            Some(int) => Align32::try_from(int),
            None => Err(if value.count_ones() == 1 {
                AlignTryFromError::TooLargePowerOfTwo
            } else {
                AlignTryFromError::NotAPowerOfTwo
            }),
        }
    }
}

impl TryFrom<&BigInt> for Align32 {
    type Error = AlignTryFromError;

    fn try_from(value: &BigInt) -> Result<Align32, AlignTryFromError> {
        if let Sign::Minus = value.sign() {
            Err(AlignTryFromError::NotAPowerOfTwo)
        } else {
            Align32::try_from(value.magnitude())
        }
    }
}

//===========================================================================//

/// The error for when a conversion to [Align32] fails.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AlignTryFromError {
    /// The value to be converted wasn't a power of two, and so can't be used
    /// as an alignment.
    NotAPowerOfTwo,
    /// The value to be converted was too large a power of two to fit in an
    /// [Align32].
    TooLargePowerOfTwo,
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::Align32;
    use crate::obj::BinaryIo;
    use std::fmt::Debug;
    use std::mem::size_of;

    fn round_trip<T: BinaryIo + Debug + Eq>(original: T) {
        let mut data: Vec<u8> = Vec::new();
        original.write_to(&mut data).expect("write_to");
        let parsed = T::read_from(&mut data.as_slice()).expect("read_from");
        assert_eq!(parsed, original);
    }

    #[test]
    fn binary_io_round_trip() {
        for i in 0..32 {
            round_trip(Align32::try_from(1 << i).unwrap());
            round_trip(Some(Align32::try_from(1 << i).unwrap()));
        }
        round_trip(Option::<Align32>::None);
    }

    #[test]
    fn default() {
        assert_eq!(u32::from(Align32::default()), 1);
    }

    #[test]
    fn format() {
        let align = Align32::try_from(64u32).unwrap();
        assert_eq!(format!("{:?}", align), "Align32(64)");
        assert_eq!(format!("{}", align), "64");
        assert_eq!(format!("${:04x}", align), "$0040");
        assert_eq!(format!("{:#X}", align), "0x40");
        assert_eq!(format!("0{:o}", align), "0100");
        assert_eq!(format!("%{:08b}", align), "%01000000");
    }

    #[test]
    fn efficient_option() {
        assert_eq!(size_of::<Option<Align32>>(), size_of::<Align32>());
    }
}

//===========================================================================//
