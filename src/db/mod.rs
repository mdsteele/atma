//! Facilities for simulating and debugging compiled binaries.

mod ads;
mod env;
mod error;
mod inst;
mod load;
mod prog;
mod system;

pub use ads::AdsEnvironment;
pub use error::{
    AdsError, AdsResult, AdsRuntimeError, AdsSrcContext, AdsSrcLoc,
    AdsSrcParent,
};
pub use load::load_binary;
pub use system::SimSystem;

//===========================================================================//
