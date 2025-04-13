mod mos6502;

pub use mos6502::Mos6502;

//===========================================================================//

/// An error that can occur during simulation.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SimErr {
    /// Tried to execute an invalid opcode.
    InvalidOpcode(u8),
    // TODO: support breakpoints
}

//===========================================================================//

/// A simulated processor.
pub trait SimProc {
    /// Returns a human-readable description of this simulated processor.
    fn description(&self) -> String;

    /// Returns the current address of the program counter.
    fn pc(&self) -> u32;

    /// Advances this processor by one instruction.
    fn step(&mut self) -> Result<(), SimErr>;
}

//===========================================================================//
