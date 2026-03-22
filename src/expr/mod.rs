//! Facilities for manipulating expression types and values.

mod binop;
mod check;
mod error;
mod label;
mod unop;
mod value;

pub(crate) use binop::ExprBinOp;
pub(crate) use check::{ExprCompiler, ExprEnv, ExprOp};
pub use error::{ExprEvalError, ExprTypeError, ExprTypeResult};
pub use label::ExprLabel;
pub(crate) use unop::ExprUnOp;
pub use value::{ExprType, ExprValue};

//===========================================================================//
