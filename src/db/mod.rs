//! Facilities for simulating and debugging compiled binaries.

mod ads;
mod env;
mod error;
mod expr;
mod inst;
mod load;
mod prog;

pub use ads::AdsEnvironment;
pub use env::SimEnv;
pub use error::{
    AdsError, AdsResult, AdsRuntimeError, AdsSrcContext, AdsSrcLoc,
    AdsSrcParent,
};
pub use load::load_binary;

//===========================================================================//
