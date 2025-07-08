use super::SimBus;

//===========================================================================//

/// A simulated read-only memory bus.  Reads beyond the ROM size will be
/// mirrored.  All writes will be ignored.
pub struct RomBus {
    rom: Box<[u8]>,
}

impl RomBus {
    /// Returns a new simulated ROM bus using the given byte array as the
    /// contents of ROM.  Panics if the length of the byte array is not a power
    /// of 2.
    pub fn new(rom: Box<[u8]>) -> RomBus {
        assert!(rom.len().is_power_of_two());
        RomBus { rom }
    }
}

impl SimBus for RomBus {
    fn description(&self) -> String {
        let size = self.rom.len();
        if size < 1024 {
            format!("{}B ROM", size)
        } else if size < 1024 * 1024 {
            format!("{}kB ROM", size >> 10)
        } else {
            format!("{}MB ROM", size >> 20)
        }
    }

    fn label_at(&self, _addr: u32) -> Option<&str> {
        None
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        self.rom[(addr as usize) & (self.rom.len() - 1)]
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        self.peek_byte(addr)
    }

    fn write_byte(&mut self, _addr: u32, _data: u8) {}
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::RomBus;
    use crate::bus::SimBus;

    #[test]
    fn description() {
        let bus = RomBus::new(Box::new([0u8; 0x10]));
        assert_eq!(bus.description(), "16B ROM");
        let bus = RomBus::new(Box::new([0u8; 0x1000]));
        assert_eq!(bus.description(), "4kB ROM");
        let bus = RomBus::new(Box::new([0u8; 0x100000]));
        assert_eq!(bus.description(), "1MB ROM");
    }

    #[test]
    fn address_mirroring() {
        let mut rom = Box::new([0u8; 0x10000]);
        rom[0x1233] = 0xab;
        rom[0x1234] = 0xcd;
        let mut bus = RomBus::new(rom);
        assert_eq!(bus.read_byte(0x01232), 0x00);
        assert_eq!(bus.read_byte(0x11232), 0x00);
        assert_eq!(bus.read_byte(0x21232), 0x00);
        assert_eq!(bus.read_byte(0x01233), 0xab);
        assert_eq!(bus.read_byte(0x11233), 0xab);
        assert_eq!(bus.read_byte(0x21233), 0xab);
        assert_eq!(bus.read_byte(0x01234), 0xcd);
        assert_eq!(bus.read_byte(0x11234), 0xcd);
        assert_eq!(bus.read_byte(0x21234), 0xcd);
    }
}

//===========================================================================//
