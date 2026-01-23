use super::{Addr, SimBus, WatchId, WatchKind};
use bimap::BiHashMap;

//===========================================================================//

/// Returns a simulated bus, with the given number of address bits, that's not
/// connected to anything.  All writes will be ignored, and all reads will
/// return zero.
pub fn new_open_bus(addr_bits: u32) -> Box<dyn SimBus> {
    assert!(addr_bits <= 32);
    let storage = OpenBus { addr_bits };
    Box::new(RandomAccessBus::new(Box::new(storage)))
}

/// Creates a simulated RAM bus with the given initial contents.  Panics if the
/// length of the byte array is not a power of 2.
pub fn new_ram_bus(contents: Box<[u8]>) -> Box<dyn SimBus> {
    assert!(contents.len().is_power_of_two());
    let storage = RamStorage { contents, read_only: false };
    Box::new(RandomAccessBus::new(Box::new(storage)))
}

/// Creates a simulated ROM bus with the given contents.  All writes to this
/// bus will be ignored.  Panics if the length of the byte array is not a power
/// of 2.
pub fn new_rom_bus(contents: Box<[u8]>) -> Box<dyn SimBus> {
    assert!(contents.len().is_power_of_two());
    let storage = RamStorage { contents, read_only: true };
    Box::new(RandomAccessBus::new(Box::new(storage)))
}

//===========================================================================//

struct RandomAccessBus {
    storage: Box<dyn Storage>,
    watchpoints: BiHashMap<(Addr, WatchKind), WatchId>,
}

impl RandomAccessBus {
    pub fn new(storage: Box<dyn Storage>) -> RandomAccessBus {
        RandomAccessBus { storage, watchpoints: BiHashMap::new() }
    }
}

impl SimBus for RandomAccessBus {
    fn description(&self) -> String {
        self.storage.description()
    }

    fn label_at(&self, _addr: Addr) -> Option<&str> {
        None
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let addr = addr & self.storage.addr_mask();
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let addr = addr & self.storage.addr_mask();
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

    fn peek_byte(&self, addr: Addr) -> u8 {
        self.storage.read_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        self.storage.read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        self.storage.write_byte(addr, data);
    }
}

//===========================================================================//

trait Storage {
    fn description(&self) -> String;
    fn addr_mask(&self) -> Addr;
    fn read_byte(&self, addr: Addr) -> u8;
    fn write_byte(&mut self, addr: Addr, data: u8);
}

struct OpenBus {
    addr_bits: u32,
}

impl Storage for OpenBus {
    fn description(&self) -> String {
        format!("{}-bit open bus", self.addr_bits)
    }

    fn addr_mask(&self) -> Addr {
        debug_assert!(self.addr_bits <= 32);
        Addr::wrap_usize((1usize << self.addr_bits) - 1)
    }

    fn read_byte(&self, _addr: Addr) -> u8 {
        0
    }

    fn write_byte(&mut self, _addr: Addr, _data: u8) {}
}

struct RamStorage {
    contents: Box<[u8]>,
    read_only: bool,
}

impl Storage for RamStorage {
    fn description(&self) -> String {
        let byte_size = self.contents.len();
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

    fn addr_mask(&self) -> Addr {
        debug_assert!(self.contents.len().is_power_of_two());
        Addr::wrap_usize(self.contents.len() - 1)
    }

    fn read_byte(&self, addr: Addr) -> u8 {
        let addr = addr & self.addr_mask();
        self.contents[addr.as_usize()]
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        if !self.read_only {
            let addr = addr & self.addr_mask();
            self.contents[addr.as_usize()] = data;
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{Addr, new_ram_bus, new_rom_bus};

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
        assert_eq!(bus.read_byte(Addr::from(0x01234u32)), 0x00);
        assert_eq!(bus.read_byte(Addr::from(0x11234u32)), 0x00);
        assert_eq!(bus.read_byte(Addr::from(0x21234u32)), 0x00);
        bus.write_byte(Addr::from(0x01234u32), 0xab);
        assert_eq!(bus.read_byte(Addr::from(0x01235u32)), 0x00);
        assert_eq!(bus.read_byte(Addr::from(0x01234u32)), 0xab);
        assert_eq!(bus.read_byte(Addr::from(0x11234u32)), 0xab);
        assert_eq!(bus.read_byte(Addr::from(0x21234u32)), 0xab);
        bus.write_byte(Addr::from(0x71234u32), 0xcd);
        assert_eq!(bus.read_byte(Addr::from(0x01235u32)), 0x00);
        assert_eq!(bus.read_byte(Addr::from(0x01234u32)), 0xcd);
        assert_eq!(bus.read_byte(Addr::from(0x11234u32)), 0xcd);
        assert_eq!(bus.read_byte(Addr::from(0x21234u32)), 0xcd);
    }
}

//===========================================================================//
