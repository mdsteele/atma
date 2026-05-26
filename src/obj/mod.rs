//! Facilities for representing assembled object files.

mod assert;
mod binary;
mod chunk;
mod expr;
mod file;
mod patch;
mod symbol;

pub use assert::ObjAssert;
pub use binary::BinaryIo;
pub use chunk::ObjChunk;
pub use expr::ObjExpr;
pub(crate) use expr::ObjExprOp;
pub use file::ObjFile;
pub use patch::{ObjPatch, ObjPatchData, ObjPatchIntType};
pub use symbol::ObjSymbol;

#[cfg(test)]
pub(crate) use binary::assert_round_trips;

//===========================================================================//
