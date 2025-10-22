//! Facilities for representing assembled object files.

mod align;
mod binary;
mod chunk;

pub use align::Align32;
pub use binary::BinaryIo;
pub use chunk::ObjectChunk;

//===========================================================================//
