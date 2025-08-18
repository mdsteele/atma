//! Facilities for simulating and debugging compiled binaries.

mod ads;
mod binop;
mod breakpoint;
mod env;
mod expr;
mod load;
mod value;

pub use ads::{AdsEnvironment, AdsProgram, AdsRuntimeError};
pub use env::SimEnv;
pub use load::load_binary;
pub use value::{AdsType, AdsValue};

//===========================================================================//
