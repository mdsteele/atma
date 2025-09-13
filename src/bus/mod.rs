//! Facilities for representing a memory mapping and simulating a memory bus.

use std::io::{self, Read};
use std::sync::atomic::{AtomicU64, Ordering};

mod label;
mod mmc3;
mod nes;
mod ram;

pub use label::LabeledBus;
pub use mmc3::Mmc3Bus;
pub use nes::NesBus;
pub use ram::{new_open_bus, new_ram_bus, new_rom_bus};

//===========================================================================//

/// Unique identifier for a watchpoint within a simulated memory bus.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct WatchId(u64);

impl WatchId {
    /// Creates a new [WatchId] that is different from any other created so
    /// far.
    pub fn create() -> WatchId {
        static NEXT_TAG: AtomicU64 = AtomicU64::new(0);
        WatchId(NEXT_TAG.fetch_add(1, Ordering::Relaxed))
    }
}

//===========================================================================//

/// Kinds of watchpoints that can be set on a simulated memory bus.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum WatchKind {
    /// Watch for the PC reaching the instruction at the specified location.
    Pc,
    /// Watch for reading data from the specified location.
    Read,
    /// Watch for writing data to the specified location.
    Write,
}

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

    /// Returns a watchpoint for the given address, if there is one.
    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId>;

    /// Sets a watchpoint on the given address.
    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId;

    /// Sets a watchpoint at the given label, if it exists.
    fn watch_label(&mut self, label: &str, kind: WatchKind)
    -> Option<WatchId>;

    /// Removes the specified watchpoint from the bus.
    fn unwatch(&mut self, id: WatchId);

    /// Returns the value of a single byte in memory, if the processor were to
    /// read it, but without triggering any watchpoints or performing any side
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
    /// bus represents read-only memory), and/or have other side effects.
    fn write_byte(&mut self, addr: u32, data: u8);
}

//===========================================================================//

pub(crate) struct BusPeeker<'a> {
    bus: &'a dyn SimBus,
    addr: u32,
}

impl<'a> BusPeeker<'a> {
    pub fn new(bus: &'a dyn SimBus, start_addr: u32) -> BusPeeker<'a> {
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

#[cfg(test)]
mod tests {
    use super::WatchId;

    #[test]
    fn create_watch_id() {
        let id1 = WatchId::create();
        let id2 = WatchId::create();
        let id3 = id1;
        assert_ne!(id1, id2);
        assert_eq!(id1, id3);
    }
}

//===========================================================================//
