//! Facilities for representing a memory mapping and simulating a memory bus.

use std::io::{self, Read};

mod label;
mod null;
mod ram;

pub use label::LabeledBus;
pub use null::NullBus;
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

    /// Returns a label for the given address, if there is one.
    fn label_at(&self, addr: u32) -> Option<&str>;

    /// Returns the value of a single byte in memory, if the processor were to
    /// read it, but without triggering any breakpoints or performing any side
    /// effects that would occur if the processor actually read the byte.
    fn peek_byte(&self, addr: u32) -> u8;

    /// Reads a single byte from memory.
    ///
    /// Note that this is a `&mut self` method, since some hardware registers
    /// may have side effects when read.
    fn read_byte(&mut self, addr: u32) -> u8;

    /// Writes a single byte to memory.
    ///
    /// Depending on the implementation, the write may be ignored (e.g. if this
    /// bus represents read-only memory).
    fn write_byte(&mut self, addr: u32, data: u8);
}

//===========================================================================//

pub(crate) struct BusPeeker<'a> {
    bus: &'a dyn SimBus,
    addr: u32,
}

impl BusPeeker<'_> {
    pub fn new(bus: &dyn SimBus, start_addr: u32) -> BusPeeker {
        BusPeeker { bus, addr: start_addr }
    }
}

impl Read for BusPeeker<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        for byte in buf.iter_mut() {
            *byte = self.bus.peek_byte(self.addr);
            self.addr = self.addr.wrapping_add(1);
        }
        Ok(buf.len())
    }
}

//===========================================================================//
