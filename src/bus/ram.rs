use super::SimBus;

//===========================================================================//

/// A simulated RAM bus.  Reads beyond the RAM size will be mirrored.
pub struct RamBus {
    ram: Box<[u8]>,
}

impl RamBus {
    /// Returns a new simulated RAM bus using the given byte array as the
    /// contents of RAM.  Panics if the length of the byte array is not a power
    /// of 2.
    pub fn new(ram: Box<[u8]>) -> RamBus {
        assert!(ram.len().is_power_of_two());
        RamBus { ram }
    }
}

impl SimBus for RamBus {
    fn description(&self) -> String {
        let size = self.ram.len();
        if size < 1024 {
            format!("{size}B RAM")
        } else if size < 1024 * 1024 {
            format!("{}kB RAM", size >> 10)
        } else {
            format!("{}MB RAM", size >> 20)
        }
    }

    fn label_at(&self, _addr: u32) -> Option<&str> {
        None
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        self.ram[(addr as usize) & (self.ram.len() - 1)]
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        self.peek_byte(addr)
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        self.ram[(addr as usize) & (self.ram.len() - 1)] = data;
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::RamBus;
    use crate::bus::SimBus;

    #[test]
    fn description() {
        let bus = RamBus::new(Box::new([0u8; 0x10]));
        assert_eq!(bus.description(), "16B RAM");
        let bus = RamBus::new(Box::new([0u8; 0x1000]));
        assert_eq!(bus.description(), "4kB RAM");
        let bus = RamBus::new(Box::new([0u8; 0x100000]));
        assert_eq!(bus.description(), "1MB RAM");
    }

    #[test]
    fn address_mirroring() {
        let mut bus = RamBus::new(Box::new([0u8; 0x10000]));
        assert_eq!(bus.read_byte(0x01234), 0x00);
        assert_eq!(bus.read_byte(0x11234), 0x00);
        assert_eq!(bus.read_byte(0x21234), 0x00);
        bus.write_byte(0x01234, 0xab);
        assert_eq!(bus.read_byte(0x01234), 0xab);
        assert_eq!(bus.read_byte(0x11234), 0xab);
        assert_eq!(bus.read_byte(0x21234), 0xab);
        bus.write_byte(0x71234, 0xcd);
        assert_eq!(bus.read_byte(0x01234), 0xcd);
        assert_eq!(bus.read_byte(0x11234), 0xcd);
        assert_eq!(bus.read_byte(0x21234), 0xcd);
    }
}

//===========================================================================//
