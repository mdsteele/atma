use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;
use std::fmt;

//===========================================================================//

/// Represents a nonnegative offset from one memory address to another.
#[derive(
    Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd,
)]
pub struct Offset(pub(super) u32);

impl Offset {
    /// An offset of zero.
    pub const ZERO: Offset = Offset(0);

    /// The smallest offset value (zero).
    pub const MIN: Offset = Offset(0);

    /// The largest offset value, equal to `Addr::MAX - Addr::MIN`.
    pub const MAX: Offset = Offset(u32::MAX);
}

impl From<u8> for Offset {
    fn from(value: u8) -> Offset {
        Offset(value.into())
    }
}

impl From<u16> for Offset {
    fn from(value: u16) -> Offset {
        Offset(value.into())
    }
}

impl From<u32> for Offset {
    fn from(value: u32) -> Offset {
        Offset(value)
    }
}

impl TryFrom<u64> for Offset {
    type Error = ();

    fn try_from(value: u64) -> Result<Offset, ()> {
        match u32::try_from(value) {
            Ok(int) => Ok(Offset(int)),
            Err(_) => Err(()),
        }
    }
}

impl TryFrom<usize> for Offset {
    type Error = ();

    fn try_from(value: usize) -> Result<Offset, ()> {
        match u32::try_from(value) {
            Ok(int) => Ok(Offset(int)),
            Err(_) => Err(()),
        }
    }
}

impl TryFrom<&BigUint> for Offset {
    type Error = ();

    fn try_from(value: &BigUint) -> Result<Offset, ()> {
        match value.to_u32() {
            Some(int) => Ok(Offset(int)),
            None => Err(()),
        }
    }
}

impl TryFrom<&BigInt> for Offset {
    type Error = ();

    fn try_from(value: &BigInt) -> Result<Offset, ()> {
        match value.to_u32() {
            Some(int) => Ok(Offset(int)),
            None => Err(()),
        }
    }
}

impl From<Offset> for u64 {
    fn from(value: Offset) -> u64 {
        u64::from(value.0)
    }
}

impl From<Offset> for BigUint {
    fn from(value: Offset) -> BigUint {
        BigUint::from(value.0)
    }
}

impl From<Offset> for BigInt {
    fn from(value: Offset) -> BigInt {
        BigInt::from(value.0)
    }
}

impl TryFrom<Offset> for usize {
    type Error = ();

    fn try_from(value: Offset) -> Result<usize, ()> {
        usize::try_from(value.0).map_err(|_| ())
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Binary for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Octal for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::LowerHex for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::UpperHex for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

//===========================================================================//
