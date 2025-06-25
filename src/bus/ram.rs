use super::SimBus;

//===========================================================================//

/// A simulated 16-bit memory bus connected to 64 KiB of RAM.
pub struct Ram64k {
    ram: Box<[u8; 0x10000]>,
}

impl Ram64k {
    /// Returns a new simulated 64K RAM bus using the given initial RAM
    /// contents.
    pub fn new(ram: Box<[u8; 0x10000]>) -> Ram64k {
        Ram64k { ram }
    }
}

impl SimBus for Ram64k {
    fn description(&self) -> String {
        "64k RAM".to_string()
    }

    fn label_at(&self, _addr: u32) -> Option<&str> {
        None
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        self.ram[(addr & 0xffff) as usize]
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        self.peek_byte(addr)
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        self.ram[(addr & 0xffff) as usize] = data;
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{Ram64k, SimBus};

    #[test]
    fn address_mirroring() {
        let mut bus = Ram64k::new(Box::new([0u8; 0x10000]));
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
