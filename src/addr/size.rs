use super::{Addr, Align};
use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;
use std::fmt;
use std::ops;

//===========================================================================//

/// Represents the size of a region of bus memory, in bytes.
#[derive(
    Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd,
)]
pub struct Size(pub(super) u64);

impl Size {
    /// A size of zero.
    pub const ZERO: Size = Size(0);

    /// The smallest size value (zero).
    pub const MIN: Size = Size(0);

    const MAX_INTERNAL: u64 = 1u64 << Addr::BITS;

    /// The largest possible size value, `1 << Addr::BITS`.
    pub const MAX: Size = Size(Size::MAX_INTERNAL);

    /// Returns the size of a range of addresses.
    pub fn of_range(range: &ops::RangeInclusive<Addr>) -> Size {
        if range.is_empty() {
            Size::ZERO
        } else {
            Size((range.end().0 - range.start().0) as u64 + 1)
        }
    }

    // This is *internal-only*, and will need to be removed if we ever increase
    // `Addr::BITS` to 64.
    pub(crate) fn into_u64(self) -> u64 {
        self.0
    }
}

impl From<Align> for Size {
    fn from(align: Align) -> Size {
        Size(1 << align.log2())
    }
}

impl From<u8> for Size {
    fn from(value: u8) -> Size {
        Size(value.into())
    }
}

impl From<u16> for Size {
    fn from(value: u16) -> Size {
        Size(value.into())
    }
}

impl From<u32> for Size {
    fn from(value: u32) -> Size {
        Size(value.into())
    }
}

impl TryFrom<u64> for Size {
    type Error = ();

    fn try_from(value: u64) -> Result<Size, ()> {
        if value <= Size::MAX_INTERNAL { Ok(Size(value)) } else { Err(()) }
    }
}

impl TryFrom<usize> for Size {
    type Error = ();

    fn try_from(value: usize) -> Result<Size, ()> {
        Size::try_from(value as u64)
    }
}

impl TryFrom<&BigUint> for Size {
    type Error = ();

    fn try_from(value: &BigUint) -> Result<Size, ()> {
        match value.to_u64() {
            Some(int) => Size::try_from(int),
            None => Err(()),
        }
    }
}

impl TryFrom<&BigInt> for Size {
    type Error = ();

    fn try_from(value: &BigInt) -> Result<Size, ()> {
        match value.to_u64() {
            Some(int) => Size::try_from(int),
            None => Err(()),
        }
    }
}

impl From<Size> for BigUint {
    fn from(value: Size) -> BigUint {
        BigUint::from(value.0)
    }
}

impl From<Size> for BigInt {
    fn from(value: Size) -> BigInt {
        BigInt::from(value.0)
    }
}

impl TryFrom<Size> for u64 {
    type Error = ();

    // Currently this always succeeds, but that could change in the future if
    // we ever want to increase `Addr::BITS` to 64.
    fn try_from(value: Size) -> Result<u64, ()> {
        Ok(value.0)
    }
}

impl TryFrom<Size> for usize {
    type Error = ();

    fn try_from(value: Size) -> Result<usize, ()> {
        usize::try_from(value.0).map_err(|_| ())
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Binary for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Octal for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::LowerHex for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::UpperHex for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

//===========================================================================//
