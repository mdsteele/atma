//! Facilities for representing assembled object files.

mod assert;
mod binary;
mod chunk;
mod context;
mod expr;
mod file;
mod import;
mod patch;
mod symbol;

pub use assert::ObjAssert;
pub use binary::{BinaryIo, Decoder, Encoder};
pub use chunk::ObjChunk;
pub use context::{ObjSrcContext, ObjSrcLoc, ObjSrcParent};
pub use expr::ObjExpr;
pub(crate) use expr::ObjExprOp;
pub use file::ObjFile;
pub use import::ObjImport;
pub use patch::{ObjPatch, ObjPatchData, ObjPatchIntType};
pub use symbol::ObjSymbol;

#[cfg(test)]
pub(crate) use binary::assert_round_trips;

//===========================================================================//
