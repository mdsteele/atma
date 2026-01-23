use super::{Addr, SimBus, WatchId, WatchKind, new_open_bus, new_ram_bus};

//===========================================================================//

/// A simulated classic Game Boy (DMG) CPU memory bus.
///
/// See <https://gbdev.io/pandocs/Memory_Map.html> for details.
pub struct DmgBus {
    cart: Box<dyn SimBus>,
    vram: Box<dyn SimBus>,
    wram: Box<dyn SimBus>,
    oam: Box<dyn SimBus>,
    unused: Box<dyn SimBus>,
    io_regs: Box<dyn SimBus>,
    hiram: Box<dyn SimBus>,
}

impl DmgBus {
    /// Constructs a simulated classic Game Boy (DMG) CPU memory bus, given the
    /// memory bus for the simulated game cartridge.
    pub fn with_cartridge(cartridge_bus: Box<dyn SimBus>) -> DmgBus {
        DmgBus {
            cart: cartridge_bus,
            vram: new_ram_bus(Box::new([0u8; 0x2000])),
            wram: new_ram_bus(Box::new([0u8; 0x2000])),
            oam: new_ram_bus(Box::new([0u8; 0x100])),
            unused: new_open_bus(16),
            io_regs: new_ram_bus(Box::new([0u8; 0x80])), // TODO
            hiram: new_ram_bus(Box::new([0u8; 0x80])),
        }
    }
}

impl SimBus for DmgBus {
    fn description(&self) -> String {
        format!("DMG with {}", self.cart.description())
    }

    fn label_at(&self, addr: Addr) -> Option<&str> {
        match addr.as_u16() {
            0x0000..0x8000 => self.cart.label_at(addr),
            0x8000..0xa000 => self.vram.label_at(addr),
            0xa000..0xc000 => self.cart.label_at(addr),
            0xc000..0xfe00 => self.wram.label_at(addr),
            0xfe00..0xfea0 => self.oam.label_at(addr),
            0xfea0..0xff00 => self.unused.label_at(addr),
            0xff00..0xff80 => self.io_regs.label_at(addr),
            0xff80..0xffff => self.hiram.label_at(addr),
            0xffff => Some("IE"),
        }
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        match addr.as_u16() {
            0x0000..0x8000 => self.cart.watchpoint_at(addr, kind),
            0x8000..0xa000 => self.vram.watchpoint_at(addr, kind),
            0xa000..0xc000 => self.cart.watchpoint_at(addr, kind),
            0xc000..0xfe00 => self.wram.watchpoint_at(addr, kind),
            0xfe00..0xfea0 => self.oam.watchpoint_at(addr, kind),
            0xfea0..0xff00 => self.unused.watchpoint_at(addr, kind),
            0xff00..0xff80 => self.io_regs.watchpoint_at(addr, kind),
            0xff80.. => self.hiram.watchpoint_at(addr, kind),
        }
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        match addr.as_u16() {
            0x0000..0x8000 => self.cart.watch_address(addr, kind),
            0x8000..0xa000 => self.vram.watch_address(addr, kind),
            0xa000..0xc000 => self.cart.watch_address(addr, kind),
            0xc000..0xfe00 => self.wram.watch_address(addr, kind),
            0xfe00..0xfea0 => self.oam.watch_address(addr, kind),
            0xfea0..0xff00 => self.unused.watch_address(addr, kind),
            0xff00..0xff80 => self.io_regs.watch_address(addr, kind),
            0xff80.. => self.hiram.watch_address(addr, kind),
        }
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.cart
            .watch_label(label, kind)
            .or_else(|| self.wram.watch_label(label, kind))
            .or_else(|| self.hiram.watch_label(label, kind))
            .or_else(|| self.vram.watch_label(label, kind))
            .or_else(|| self.oam.watch_label(label, kind))
            .or_else(|| self.io_regs.watch_label(label, kind))
            .or_else(|| {
                if label == "IE" {
                    Some(self.hiram.watch_address(Addr::from(0xffffu16), kind))
                } else {
                    None
                }
            })
    }

    fn unwatch(&mut self, id: WatchId) {
        self.cart.unwatch(id);
        self.vram.unwatch(id);
        self.wram.unwatch(id);
        self.oam.unwatch(id);
        self.unused.unwatch(id);
        self.io_regs.unwatch(id);
        self.hiram.unwatch(id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        match addr.as_u16() {
            0x0000..0x8000 => self.cart.peek_byte(addr),
            0x8000..0xa000 => self.vram.peek_byte(addr),
            0xa000..0xc000 => self.cart.peek_byte(addr),
            0xc000..0xfe00 => self.wram.peek_byte(addr),
            0xfe00..0xfea0 => self.oam.peek_byte(addr),
            0xfea0..0xff00 => self.unused.peek_byte(addr),
            0xff00..0xff80 => self.io_regs.peek_byte(addr),
            0xff80.. => self.hiram.peek_byte(addr),
        }
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        match addr.as_u16() {
            0x0000..0x8000 => self.cart.read_byte(addr),
            0x8000..0xa000 => self.vram.read_byte(addr),
            0xa000..0xc000 => self.cart.read_byte(addr),
            0xc000..0xfe00 => self.wram.read_byte(addr),
            0xfe00..0xfea0 => self.oam.read_byte(addr),
            0xfea0..0xff00 => self.unused.read_byte(addr),
            0xff00..0xff80 => self.io_regs.read_byte(addr),
            0xff80.. => self.hiram.read_byte(addr),
        }
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        match addr.as_u16() {
            0x0000..0x8000 => self.cart.write_byte(addr, data),
            0x8000..0xa000 => self.vram.write_byte(addr, data),
            0xa000..0xc000 => self.cart.write_byte(addr, data),
            0xc000..0xfe00 => self.wram.write_byte(addr, data),
            0xfe00..0xfea0 => self.oam.write_byte(addr, data),
            0xfea0..0xff00 => self.unused.write_byte(addr, data),
            0xff00..0xff80 => self.io_regs.write_byte(addr, data),
            0xff80.. => self.hiram.write_byte(addr, data),
        }
    }
}

//===========================================================================//
