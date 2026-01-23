use num_bigint::BigInt;
use std::fmt;
use std::ops;

//===========================================================================//

/// Represents a memory bus address.
#[derive(
    Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd,
)]
pub struct Addr(u32);

impl Addr {
    /// Converts a `BigInt` into an `Addr`. If the value of the `BigInt` is
    /// outside the range of the `Addr`, that value is wrapped.
    pub fn wrap_bigint(value: &BigInt) -> Addr {
        Addr(u32::try_from(value & BigInt::from(0xffffffffu32)).unwrap())
    }

    /// Converts a `usize` into an `Addr`. If the value of the `usize` is
    /// outside the range of the `Addr`, that value is wrapped.
    pub fn wrap_usize(value: usize) -> Addr {
        Addr(value as u32)
    }

    /// Returns the lowest 8 bits of the address.
    pub fn as_u8(self) -> u8 {
        self.0 as u8
    }

    /// Returns the lowest 16 bits of the address.
    pub fn as_u16(self) -> u16 {
        self.0 as u16
    }

    /// Returns the lowest 32 bits of the address.
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Converts the address into a `usize`.
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<u8> for Addr {
    fn from(value: u8) -> Addr {
        Addr(value.into())
    }
}

impl From<u16> for Addr {
    fn from(value: u16) -> Addr {
        Addr(value.into())
    }
}

impl From<u32> for Addr {
    fn from(value: u32) -> Addr {
        Addr(value)
    }
}

impl From<Addr> for BigInt {
    fn from(value: Addr) -> BigInt {
        BigInt::from(value.0)
    }
}

impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Binary for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Octal for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::LowerHex for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::UpperHex for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl ops::Add<u32> for Addr {
    type Output = Addr;

    fn add(self, rhs: u32) -> Addr {
        Addr(self.0.wrapping_add(rhs))
    }
}

impl ops::AddAssign<u32> for Addr {
    fn add_assign(&mut self, rhs: u32) {
        *self = *self + rhs;
    }
}

impl ops::BitAnd for Addr {
    type Output = Addr;

    fn bitand(self, rhs: Addr) -> Addr {
        Addr(self.0 & rhs.0)
    }
}

impl ops::BitOr for Addr {
    type Output = Addr;

    fn bitor(self, rhs: Addr) -> Addr {
        Addr(self.0 | rhs.0)
    }
}

impl ops::Shl<u32> for Addr {
    type Output = Addr;

    fn shl(self, rhs: u32) -> Addr {
        Addr(self.0.unbounded_shl(rhs))
    }
}

impl ops::Shr<u32> for Addr {
    type Output = Addr;

    fn shr(self, rhs: u32) -> Addr {
        Addr(self.0.unbounded_shr(rhs))
    }
}

//===========================================================================//
