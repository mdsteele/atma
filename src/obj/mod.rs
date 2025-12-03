//! Facilities for representing assembled object files.

mod align;
mod binary;
mod chunk;
mod expr;
mod file;
mod patch;
mod symbol;

pub use align::Align32;
pub use binary::BinaryIo;
pub use chunk::ObjectChunk;
pub use expr::ObjExpr;
pub(crate) use expr::ObjExprOp;
pub use file::ObjectFile;
pub use patch::{ObjectPatch, PatchKind};
pub use symbol::ObjectSymbol;

#[cfg(test)]
pub(crate) use binary::assert_round_trips;

//===========================================================================//
