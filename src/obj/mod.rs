//! Facilities for representing assembled object files.

mod align;
mod binary;
mod chunk;
mod file;
mod symbol;

pub use align::Align32;
pub use binary::BinaryIo;
pub use chunk::ObjectChunk;
pub use file::ObjectFile;
pub use symbol::ObjectSymbol;

#[cfg(test)]
pub(crate) use binary::assert_round_trips;

//===========================================================================//
