//! Facilities for manipulating expression types and values.

mod binop;
mod check;
mod unop;
mod value;

pub(crate) use binop::ExprBinOp;
pub(crate) use check::{ExprCompiler, ExprEnv, ExprOp};
pub(crate) use unop::ExprUnOp;
pub use value::{ExprType, ExprValue};

//===========================================================================//
