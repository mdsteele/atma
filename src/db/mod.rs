//! Facilities for debugging a simulated system.

mod ads;
mod env;
mod error;
mod inst;
mod prog;

pub use ads::AdsEnvironment;
pub use error::{
    AdsError, AdsResult, AdsRuntimeError, AdsSrcContext, AdsSrcLoc,
    AdsSrcParent,
};

//===========================================================================//
