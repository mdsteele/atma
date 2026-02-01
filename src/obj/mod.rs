//! Facilities for representing assembled object files.

mod binary;
mod chunk;
mod expr;
mod file;
mod patch;
mod symbol;

pub use binary::BinaryIo;
pub use chunk::ObjChunk;
pub use expr::ObjExpr;
pub(crate) use expr::ObjExprOp;
pub use file::ObjFile;
pub use patch::{ObjPatch, PatchKind};
pub use symbol::ObjSymbol;

#[cfg(test)]
pub(crate) use binary::assert_round_trips;

//===========================================================================//
