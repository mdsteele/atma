//! Types for representing bus addresses.

#[allow(clippy::module_inception)]
mod addr;
mod align;
mod endian;
mod offset;
mod range;
mod size;

pub use addr::Addr;
pub use align::{Align, AlignTryFromError};
pub use endian::Endianness;
pub use offset::Offset;
pub use range::{Range, Subranges};
pub use size::Size;

//===========================================================================//
