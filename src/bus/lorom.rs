use super::{Addr, SimBus, WatchId, WatchKind, new_open_bus};

//===========================================================================//

/// Returns a simulated SNES LoRom cartridge bus.
pub fn new_lorom_bus(
    rom_bus: Box<dyn SimBus>,
    sram_bus: Option<Box<dyn SimBus>>,
) -> Box<dyn SimBus> {
    Box::new(LoRomBus { rom: rom_bus, sram: sram_bus, open: new_open_bus(24) })
}

//===========================================================================//

/// A simulated SNES LoRom cartridge bus.
struct LoRomBus {
    rom: Box<dyn SimBus>,
    sram: Option<Box<dyn SimBus>>,
    open: Box<dyn SimBus>,
}

impl LoRomBus {
    fn sub_bus(&self, addr: Addr) -> (&dyn SimBus, Addr) {
        let bank = (addr >> 16).as_u8() & 0x7f;
        let lower = (addr.as_u16() & 0x8000) == 0;
        let base = addr.as_u16() & 0x7fff;
        if lower && bank < 0x40 {
            (&*self.open, addr)
        } else if lower
            && bank >= 0x70
            && let Some(sram) = &self.sram
        {
            (&**sram, (Addr::from(bank) << 15) | Addr::from(base))
        } else {
            (&*self.rom, (Addr::from(bank) << 15) | Addr::from(base))
        }
    }

    fn sub_bus_mut(&mut self, addr: Addr) -> (&mut dyn SimBus, Addr) {
        let bank = (addr >> 16).as_u8() & 0x7f;
        let lower = (addr.as_u16() & 0x8000) == 0;
        let base = addr.as_u16() & 0x7fff;
        if lower && bank < 0x40 {
            (&mut *self.open, addr)
        } else if lower
            && bank >= 0x70
            && let Some(sram) = &mut self.sram
        {
            (&mut **sram, (Addr::from(bank) << 15) | Addr::from(base))
        } else {
            (&mut *self.rom, (Addr::from(bank) << 15) | Addr::from(base))
        }
    }
}

impl SimBus for LoRomBus {
    fn description(&self) -> String {
        if let Some(sram) = &self.sram {
            format!(
                "LoROM cartridge with {} and {}",
                self.rom.description(),
                sram.description()
            )
        } else {
            format!("LoROM cartridge with {}", self.rom.description())
        }
    }

    fn label_at(&self, addr: Addr) -> Option<&str> {
        let (bus, addr) = self.sub_bus(addr);
        bus.label_at(addr)
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let (bus, addr) = self.sub_bus(addr);
        bus.watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.rom.watch_label(label, kind).or_else(|| {
            self.sram.as_mut().and_then(|sram| sram.watch_label(label, kind))
        })
    }

    fn unwatch(&mut self, id: WatchId) {
        self.rom.unwatch(id);
        if let Some(sram) = &mut self.sram {
            sram.unwatch(id);
        }
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        let (bus, addr) = self.sub_bus(addr);
        bus.peek_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.write_byte(addr, data);
    }
}

//===========================================================================//
