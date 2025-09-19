use super::{SimBus, WatchId, WatchKind, new_ram_bus};
use bimap::BiHashMap;

//===========================================================================//

/// A simulated NES CPU memory bus.
///
/// See https://www.nesdev.org/wiki/CPU_memory_map
pub struct NesBus {
    ram: Box<dyn SimBus>,
    ppu_regs: PpuRegBus,
    cart: Box<dyn SimBus>,
}

impl NesBus {
    /// Constructs a simulated NES CPU memory bus, given the memory bus for the
    /// simulated game cartridge.  All operations for addresses `0x4020` and up
    /// will be delegated to the cartridge bus.
    pub fn with_cartridge(cartridge_bus: Box<dyn SimBus>) -> NesBus {
        NesBus {
            ram: new_ram_bus(Box::new([0u8; 0x800])),
            ppu_regs: PpuRegBus::new(),
            cart: cartridge_bus,
        }
    }
}

impl SimBus for NesBus {
    fn description(&self) -> String {
        format!("NES with {}", self.cart.description())
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.label_at(addr),
            0x2000..0x4000 => self.ppu_regs.label_at(addr),
            0x4000..0x4020 => None, // TODO: APU registers
            0x4020.. => self.cart.label_at(addr),
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.watchpoint_at(addr, kind),
            0x2000..0x4000 => self.ppu_regs.watchpoint_at(addr, kind),
            0x4000..0x4020 => None, // TODO: APU registers
            0x4020.. => self.cart.watchpoint_at(addr, kind),
        }
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.watch_address(addr, kind),
            0x2000..0x4000 => self.ppu_regs.watch_address(addr, kind),
            0x4000..0x4020 => WatchId::create(), // TODO: APU registers
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
            .or_else(|| self.ppu_regs.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.ram.unwatch(id);
        self.ppu_regs.unwatch(id);
        self.cart.unwatch(id);
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.peek_byte(addr),
            0x2000..0x4000 => self.ppu_regs.peek_byte(addr),
            0x4000..0x4020 => 0, // TODO: APU registers
            0x4020.. => self.cart.peek_byte(addr),
        }
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.read_byte(addr),
            0x2000..0x4000 => self.ppu_regs.read_byte(addr),
            0x4000..0x4020 => 0, // TODO: APU registers
            0x4020.. => self.cart.read_byte(addr),
        }
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        match addr & 0xffff {
            0x0000..0x2000 => self.ram.write_byte(addr, data),
            0x2000..0x4000 => self.ppu_regs.write_byte(addr, data),
            0x4000..0x4020 => {} // TODO: APU registers
            0x4020.. => self.cart.write_byte(addr, data),
        }
    }
}

//===========================================================================//

pub struct PpuRegBus {
    watchpoints: BiHashMap<(u32, WatchKind), WatchId>,
}

impl PpuRegBus {
    pub fn new() -> PpuRegBus {
        PpuRegBus { watchpoints: BiHashMap::new() }
    }
}

impl SimBus for PpuRegBus {
    fn description(&self) -> String {
        "PPU registers".to_string()
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match addr & 0x7 {
            0 => Some("PPUCTRL"),
            1 => Some("PPUMASK"),
            2 => Some("PPUSTATUS"),
            3 => Some("OAMADDR"),
            4 => Some("OAMDATA"),
            5 => Some("PPUSCROLL"),
            6 => Some("PPUADDR"),
            7 => Some("PPUDATA"),
            _ => unreachable!(),
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        let addr = addr & 0x7;
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        let addr = addr & 0x7;
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
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        let addr: u32 = match label {
            "PPUCTRL" => 0,
            "PPUMASK" => 1,
            "PPUSTATUS" => 2,
            "OAMADDR" => 3,
            "OAMDATA" => 4,
            "PPUSCROLL" => 5,
            "PPUADDR" => 6,
            "PPUDATA" => 7,
            _ => return None,
        };
        Some(self.watch_address(addr, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn read_byte(&mut self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn write_byte(&mut self, _addr: u32, _data: u8) {
        // TODO
    }
}

//===========================================================================//
