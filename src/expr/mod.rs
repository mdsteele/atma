//! Facilities for manipulating expression types and values.

mod binop;
mod check;
mod value;

pub(crate) use binop::ExprBinOp;
pub(crate) use check::{ExprCompiler, ExprEnv, ExprOp};
pub use value::{ExprType, ExprValue};

//===========================================================================//
