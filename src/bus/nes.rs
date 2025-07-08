use super::{RamBus, SimBus};

//===========================================================================//

/// A simulated NES CPU memory bus.
pub struct NesBus {
    ram: RamBus,
    cart: Box<dyn SimBus>,
}

impl NesBus {
    /// Constructs a simulated NES CPU memory bus, given the memory bus for the
    /// simulated game cartridge.  All operations for addresses `0x4020` and up
    /// will be delegated to the cartridge bus.
    pub fn with_cartridge(cartridge_bus: Box<dyn SimBus>) -> NesBus {
        NesBus {
            ram: RamBus::new(Box::new([0u8; 0x800])),
            cart: cartridge_bus,
        }
    }
}

impl SimBus for NesBus {
    fn description(&self) -> String {
        format!("NES with {}", self.cart.description())
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        // See https://www.nesdev.org/wiki/CPU_memory_map
        match addr {
            0..0x2000 => self.ram.label_at(addr),
            0x2000..0x4000 => match addr % 8 {
                0 => Some("PPUCTRL"),
                1 => Some("PPUMASK"),
                2 => Some("PPUSTATUS"),
                3 => Some("OAMADDR"),
                4 => Some("OAMDATA"),
                5 => Some("PPUSCROLL"),
                6 => Some("PPUADDR"),
                7 => Some("PPUDATA"),
                _ => unreachable!(),
            },
            0x4000..0x4020 => None, // TODO
            0x4020.. => self.cart.label_at(addr),
        }
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        match addr {
            0..0x2000 => self.ram.peek_byte(addr),
            0x2000..0x4020 => 0, // TODO
            0x4020.. => self.cart.peek_byte(addr),
        }
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        match addr {
            0..0x2000 => self.ram.read_byte(addr),
            0x2000..0x4020 => 0, // TODO
            0x4020.. => self.cart.read_byte(addr),
        }
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        match addr {
            0..0x2000 => self.ram.write_byte(addr, data),
            0x2000..0x4020 => {} // TODO
            0x4020.. => self.cart.write_byte(addr, data),
        }
    }
}

//===========================================================================//
