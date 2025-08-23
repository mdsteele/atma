//! Facilities for simulating and debugging compiled binaries.

mod ads;
mod binop;
mod env;
mod expr;
mod inst;
mod load;
mod prog;
mod value;

pub use ads::{AdsEnvironment, AdsRuntimeError};
pub use env::SimEnv;
pub use load::load_binary;
pub use prog::AdsProgram;
pub use value::{AdsType, AdsValue};

//===========================================================================//
