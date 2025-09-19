use super::{SimBus, WatchId, WatchKind, new_ram_bus};

//===========================================================================//

/// A simulated NES CPU memory bus.
pub struct NesBus {
    ram: Box<dyn SimBus>,
    cart: Box<dyn SimBus>,
}

impl NesBus {
    /// Constructs a simulated NES CPU memory bus, given the memory bus for the
    /// simulated game cartridge.  All operations for addresses `0x4020` and up
    /// will be delegated to the cartridge bus.
    pub fn with_cartridge(cartridge_bus: Box<dyn SimBus>) -> NesBus {
        NesBus {
            ram: new_ram_bus(Box::new([0u8; 0x800])),
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
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.label_at(addr),
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

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.watchpoint_at(addr, kind),
            0x2000..0x4020 => None, // TODO
            0x4020.. => self.cart.watchpoint_at(addr, kind),
        }
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.watch_address(addr, kind),
            0x2000..0x4020 => WatchId::create(), // TODO
            0x4020.. => self.cart.watch_address(addr, kind),
        }
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        // TODO: support watching hardware registers
        self.ram
            .watch_label(label, kind)
            .or_else(|| self.cart.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.ram.unwatch(id);
        self.cart.unwatch(id);
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.peek_byte(addr),
            0x2000..0x4020 => 0, // TODO
            0x4020.. => self.cart.peek_byte(addr),
        }
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.read_byte(addr),
            0x2000..0x4020 => 0, // TODO
            0x4020.. => self.cart.read_byte(addr),
        }
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.write_byte(addr, data),
            0x2000..0x4020 => {} // TODO
            0x4020.. => self.cart.write_byte(addr, data),
        }
    }
}

//===========================================================================//
