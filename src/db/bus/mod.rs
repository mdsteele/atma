mod ram;

pub use ram::Ram64k;

//===========================================================================//

/// A simulated memory bus.
///
/// This trait uses `u32` for addresses, but many implementations may only
/// represent a 16-bit or 24-bit address bus. Generally speaking,
/// implementations should ignore address bits higher than the width of their
/// address bus, as though those physical address lines weren't connected,
/// effectively mirroring memory across the larger range.  Implementations
/// should *not* panic when given an address that is out of range.
pub trait SimBus {
    /// Returns a human-readable description of this simulated memory bus.
    fn description(&self) -> String;

    /// Reads a single byte from memory.
    ///
    /// Note that this is a `&mut self` method, since some hardware registers
    /// may have side effects when read.
    fn read_byte(&mut self, addr: u32) -> u8;

    /// Writes a single byte to memory.
    fn write_byte(&mut self, addr: u32, data: u8);
}

//===========================================================================//
