//! Facilities for manipulating expression types and values.

mod binop;
mod check;
mod env;
mod error;
mod func;
mod label;
mod unop;
mod value;

pub(crate) use binop::ExprBinOp;
pub(crate) use check::ExprCompiler;
pub(crate) use env::{ExprEnv, ExprOp};
pub(crate) use error::ExprStatic;
pub use error::{
    ExprEvalError, ExprNotStaticContext, ExprNotStaticReason, ExprTypeError,
    ExprTypeResult,
};
pub use func::{ExprFunc, ExprFuncEvalError};
pub use label::ExprLabel;
pub(crate) use unop::ExprUnOp;
pub use value::{ExprType, ExprValue};

//===========================================================================//
