use super::{SimBus, WatchId, WatchKind};
use bimap::BiHashMap;

//===========================================================================//

/// Creates a simulated RAM bus with the given initial contents.  Panics if the
/// length of the byte array is not a power of 2.
pub fn new_ram_bus(contents: Box<[u8]>) -> Box<dyn SimBus> {
    Box::new(RandomAccessBus::new(contents, false))
}

/// Creates a simulated ROM bus with the given contents.  All writes to this
/// bus will be ignored.  Panics if the length of the byte array is not a power
/// of 2.
pub fn new_rom_bus(contents: Box<[u8]>) -> Box<dyn SimBus> {
    Box::new(RandomAccessBus::new(contents, true))
}

//===========================================================================//

/// A simulated RAM bus.  Reads beyond the RAM size will be mirrored.
struct RandomAccessBus {
    ram: Box<[u8]>,
    watchpoints: BiHashMap<(u32, WatchKind), WatchId>,
    read_only: bool,
}

impl RandomAccessBus {
    /// Returns a new simulated RAM bus using the given byte array as the
    /// contents of RAM.  Panics if the length of the byte array is not a power
    /// of 2.
    pub fn new(ram: Box<[u8]>, read_only: bool) -> RandomAccessBus {
        assert!(ram.len().is_power_of_two());
        RandomAccessBus { ram, watchpoints: BiHashMap::new(), read_only }
    }
}

impl SimBus for RandomAccessBus {
    fn description(&self) -> String {
        let byte_size = self.ram.len();
        let (size, unit) = if byte_size < 1024 {
            (byte_size, "B")
        } else if byte_size < 1024 * 1024 {
            (byte_size >> 10, "kB")
        } else {
            (byte_size >> 20, "MB")
        };
        let kind = if self.read_only { "ROM" } else { "RAM" };
        format!("{size}{unit} {kind}")
    }

    fn label_at(&self, _addr: u32) -> Option<&str> {
        None
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        match self.watchpoints.get_by_left(&(addr, kind)) {
            Some(id) => *id,
            None => {
                let id = WatchId::create();
                self.watchpoints.insert((addr, kind), id);
                id
            }
        }
    }

    fn watch_label(
        &mut self,
        _label: &str,
        _kind: WatchKind,
    ) -> Option<WatchId> {
        None
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        self.ram[(addr as usize) & (self.ram.len() - 1)]
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        self.peek_byte(addr)
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        if !self.read_only {
            self.ram[(addr as usize) & (self.ram.len() - 1)] = data;
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{new_ram_bus, new_rom_bus};

    #[test]
    fn description() {
        let bus = new_ram_bus(Box::new([0u8; 0x10]));
        assert_eq!(bus.description(), "16B RAM");
        let bus = new_ram_bus(Box::new([0u8; 0x1000]));
        assert_eq!(bus.description(), "4kB RAM");
        let bus = new_ram_bus(Box::new([0u8; 0x100000]));
        assert_eq!(bus.description(), "1MB RAM");

        let bus = new_rom_bus(Box::new([0u8; 0x80]));
        assert_eq!(bus.description(), "128B ROM");
        let bus = new_rom_bus(Box::new([0u8; 0x8000]));
        assert_eq!(bus.description(), "32kB ROM");
    }

    #[test]
    fn address_mirroring() {
        let mut bus = new_ram_bus(Box::new([0u8; 0x10000]));
        assert_eq!(bus.read_byte(0x01234), 0x00);
        assert_eq!(bus.read_byte(0x11234), 0x00);
        assert_eq!(bus.read_byte(0x21234), 0x00);
        bus.write_byte(0x01234, 0xab);
        assert_eq!(bus.read_byte(0x01235), 0x00);
        assert_eq!(bus.read_byte(0x01234), 0xab);
        assert_eq!(bus.read_byte(0x11234), 0xab);
        assert_eq!(bus.read_byte(0x21234), 0xab);
        bus.write_byte(0x71234, 0xcd);
        assert_eq!(bus.read_byte(0x01235), 0x00);
        assert_eq!(bus.read_byte(0x01234), 0xcd);
        assert_eq!(bus.read_byte(0x11234), 0xcd);
        assert_eq!(bus.read_byte(0x21234), 0xcd);
    }
}

//===========================================================================//
