//! Facilities for simulating and debugging compiled binaries.

mod ads;
mod env;
mod error;
mod expr;
mod inst;
mod load;
mod prog;

pub use ads::{AdsEnvironment, AdsRuntimeError};
pub use env::SimEnv;
pub use error::{AdsError, AdsResult, AdsSrcContext, AdsSrcLoc, AdsSrcParent};
pub use load::load_binary;

//===========================================================================//
