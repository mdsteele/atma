//! Facilities for loading and simulating compiled binaries.

mod cc65;
mod gb;
mod gbs;
mod load;
mod nes;
mod nsf;
mod sfc;
mod sim;
mod spc;

pub use cc65::load_sim65_binary;
pub use gb::load_gb_binary;
pub use gbs::load_gbs_binary;
pub use load::{BinaryFormat, load_binary, load_binary_with_format};
pub use nes::load_nes_binary;
pub use nsf::load_nsf_binary;
pub use sfc::load_sfc_binary;
pub use sim::SimSystem;
pub use spc::load_spc_binary;

//===========================================================================//
