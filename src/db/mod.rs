//! Facilities for simulating and debugging compiled binaries.

mod ads;
mod env;
mod load;
mod proc;
mod value;

pub use ads::{AdsEnvironment, AdsProgram};
pub use env::SimEnv;
pub use load::load_binary;
pub use proc::{Breakpoint, Mos6502, SharpSm83, SimBreak, SimProc};
pub use value::{AdsType, AdsValue};

//===========================================================================//
