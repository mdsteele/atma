use super::Addr;
use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;
use static_assertions::const_assert;
use std::fmt;

//===========================================================================//

/// Represents a bit width for a bus address, between 0 and `Addr::BITS`
/// inclusive.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BitWidth(pub(super) u8);

impl BitWidth {
    /// The smallest bit width, zero.
    ///
    /// A zero-bit address bus effectively has only a single address.
    pub const MIN: BitWidth = BitWidth(0);

    /// The largest bit width, `Addr::BITS`.
    pub const MAX: BitWidth = BitWidth(Addr::BITS as u8);

    /// A convenience constant for the common case of a 16-bit width.
    pub const SIXTEEN: BitWidth = BitWidth(16);

    /// A convenience constant for the common case of a 24-bit width.
    pub const TWENTY_FOUR: BitWidth = BitWidth(24);

    /// Creates a `BitWidth` with the given number of bits.
    ///
    /// Panics if `bits > Addr::BITS`.
    pub const fn new(bits: u32) -> Self {
        assert!(bits <= Addr::BITS);
        Self(bits as u8)
    }

    /// Returns the number of hexidecimal digits needed to represent an address
    /// with this bit width.
    pub fn hex_digits(self) -> usize {
        usize::from(self.0.div_ceil(4))
    }

    /// Wraps the given address to this bit width.
    pub fn wrap(self, addr: Addr) -> Addr {
        Addr(addr.0 & ((1u64 << self.0) - 1) as u32)
    }
}

impl fmt::Debug for BitWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "BitWidth({})", self.0)
    }
}

impl fmt::Display for BitWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Binary for BitWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Octal for BitWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::LowerHex for BitWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::UpperHex for BitWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl TryFrom<i32> for BitWidth {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, ()> {
        if value >= 0 && (value as u32) <= Addr::BITS {
            Ok(Self(value as u8))
        } else {
            Err(())
        }
    }
}

impl TryFrom<u32> for BitWidth {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, ()> {
        if value <= Addr::BITS { Ok(Self(value as u8)) } else { Err(()) }
    }
}

impl TryFrom<&BigUint> for BitWidth {
    type Error = ();

    fn try_from(value: &BigUint) -> Result<Self, ()> {
        match value.to_u32() {
            Some(int) => BitWidth::try_from(int),
            None => Err(()),
        }
    }
}

impl TryFrom<&BigInt> for BitWidth {
    type Error = ();

    fn try_from(value: &BigInt) -> Result<Self, ()> {
        match value.to_u32() {
            Some(int) => BitWidth::try_from(int),
            None => Err(()),
        }
    }
}

impl From<BitWidth> for i8 {
    fn from(value: BitWidth) -> i8 {
        const_assert!(Addr::BITS <= (i8::MAX as u32));
        value.0 as i8
    }
}

impl From<BitWidth> for u8 {
    fn from(value: BitWidth) -> u8 {
        value.0
    }
}

impl From<BitWidth> for i16 {
    fn from(value: BitWidth) -> i16 {
        i16::from(value.0)
    }
}

impl From<BitWidth> for u16 {
    fn from(value: BitWidth) -> u16 {
        u16::from(value.0)
    }
}

impl From<BitWidth> for i32 {
    fn from(value: BitWidth) -> i32 {
        i32::from(value.0)
    }
}

impl From<BitWidth> for u32 {
    fn from(value: BitWidth) -> u32 {
        u32::from(value.0)
    }
}

impl From<BitWidth> for i64 {
    fn from(value: BitWidth) -> i64 {
        i64::from(value.0)
    }
}

impl From<BitWidth> for u64 {
    fn from(value: BitWidth) -> u64 {
        u64::from(value.0)
    }
}

impl From<BitWidth> for i128 {
    fn from(value: BitWidth) -> i128 {
        i128::from(value.0)
    }
}

impl From<BitWidth> for u128 {
    fn from(value: BitWidth) -> u128 {
        u128::from(value.0)
    }
}

impl From<BitWidth> for BigUint {
    fn from(value: BitWidth) -> BigUint {
        BigUint::from(value.0)
    }
}

impl From<BitWidth> for BigInt {
    fn from(value: BitWidth) -> BigInt {
        BigInt::from(value.0)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::BitWidth;
    use crate::addr::Addr;

    #[test]
    fn bit_width_format() {
        let width = BitWidth::try_from(26).unwrap();
        assert_eq!(format!("{:?}", width), "BitWidth(26)");
        assert_eq!(format!("{}", width), "26");
        assert_eq!(format!("${:04x}", width), "$001a");
        assert_eq!(format!("{:#X}", width), "0x1A");
        assert_eq!(format!("0{:o}", width), "032");
        assert_eq!(format!("%{:08b}", width), "%00011010");
    }

    #[test]
    fn wrap_addr() {
        let addr = Addr::from(0xface4321u32);
        assert_eq!(BitWidth::MAX.wrap(addr), addr);
        assert_eq!(BitWidth::MIN.wrap(addr), Addr::from(0u32));
        assert_eq!(BitWidth::SIXTEEN.wrap(addr), Addr::from(0x4321u32));
        assert_eq!(BitWidth::TWENTY_FOUR.wrap(addr), Addr::from(0xce4321u32));
        assert_eq!(BitWidth::new(9).wrap(addr), Addr::from(0x121u32));
        assert_eq!(BitWidth::new(31).wrap(addr), Addr::from(0x7ace4321u32));
    }
}

//===========================================================================/
