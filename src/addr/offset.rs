use super::Addr;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::fmt;

//===========================================================================//

/// Represents a positive or negative offset between bus memory addresses.
#[derive(
    Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd,
)]
pub struct Offset(pub(super) i64);

impl Offset {
    /// An offset of zero.
    pub const ZERO: Offset = Offset(0);

    const MAX_INTERNAL: i64 = Addr::MAX.0 as i64;
    const MIN_INTERNAL: i64 = -Offset::MAX_INTERNAL;

    /// The lowest negative offset, equivalent to `Addr::MIN - Addr::MAX`.
    pub const MIN: Offset = Offset(Offset::MIN_INTERNAL);

    /// The largest positive offset, equivalent to `Addr::MAX - Addr::MIN`.
    pub const MAX: Offset = Offset(Offset::MAX_INTERNAL);
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
        Offset(value.into())
    }
}

impl From<i8> for Offset {
    fn from(value: i8) -> Offset {
        Offset(value.into())
    }
}

impl From<i16> for Offset {
    fn from(value: i16) -> Offset {
        Offset(value.into())
    }
}

impl From<i32> for Offset {
    fn from(value: i32) -> Offset {
        Offset(value.into())
    }
}

impl From<Offset> for BigInt {
    fn from(value: Offset) -> BigInt {
        BigInt::from(value.0)
    }
}

impl TryFrom<i64> for Offset {
    type Error = ();

    fn try_from(value: i64) -> Result<Offset, ()> {
        if (Offset::MIN_INTERNAL..=Offset::MAX_INTERNAL).contains(&value) {
            Ok(Offset(value))
        } else {
            Err(())
        }
    }
}

impl TryFrom<usize> for Offset {
    type Error = ();

    fn try_from(value: usize) -> Result<Offset, ()> {
        Offset::try_from(value as i64)
    }
}

impl TryFrom<&BigInt> for Offset {
    type Error = ();

    fn try_from(value: &BigInt) -> Result<Offset, ()> {
        match value.to_i64() {
            Some(int) => Offset::try_from(int),
            None => Err(()),
        }
    }
}

impl TryFrom<Offset> for u64 {
    type Error = ();

    fn try_from(value: Offset) -> Result<u64, ()> {
        u64::try_from(value.0).map_err(|_| ())
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
