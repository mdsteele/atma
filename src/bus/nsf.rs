use super::{SimBus, WatchId, WatchKind, new_ram_bus, new_rom_bus};

//===========================================================================//

/// Returns a simulated bus for an NSF (NES Sound Format) "cartridge".
pub fn new_nsf_bus(
    rom_data: Box<[u8]>,
    init_addr: u16,
    play_addr: u16,
) -> Box<dyn SimBus> {
    Box::new(NsfBus::new(rom_data, init_addr, play_addr))
}

//===========================================================================//

struct NsfBus {
    // TODO: support bank switching
    driver: Box<dyn SimBus>,
    ram: Box<dyn SimBus>,
    rom: Box<dyn SimBus>,
}

impl NsfBus {
    fn new(rom_data: Box<[u8]>, init_addr: u16, play_addr: u16) -> NsfBus {
        let driver_data: [u8; 16] = [
            // JSR init_addr
            0x20,
            init_addr as u8,
            (init_addr >> 8) as u8,
            // JSR play_addr
            0x20,
            play_addr as u8,
            (play_addr >> 8) as u8,
            // CLV
            0xb8,
            // BVC -6
            0x50,
            0xfa,
            // NOPs
            0xea,
            0xea,
            0xea,
            0xea,
            0xea,
            0xea,
            0xea,
        ];
        NsfBus {
            driver: new_rom_bus(Box::new(driver_data)),
            ram: new_ram_bus(vec![0u8; 0x2000].into_boxed_slice()),
            rom: new_rom_bus(rom_data),
        }
    }
}

impl SimBus for NsfBus {
    fn description(&self) -> String {
        format!(
            "NSF with {} and {}",
            self.ram.description(),
            self.rom.description()
        )
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match addr & 0xffff {
            0x0000..0x6000 => self.driver.label_at(addr),
            0x6000..0x8000 => self.ram.label_at(addr),
            0x8000.. => self.rom.label_at(addr),
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        match addr & 0xffff {
            0x0000..0x6000 => self.driver.watchpoint_at(addr, kind),
            0x6000..0x8000 => self.ram.watchpoint_at(addr, kind),
            0x8000.. => self.rom.watchpoint_at(addr, kind),
        }
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        match addr & 0xffff {
            0x0000..0x6000 => self.driver.watch_address(addr, kind),
            0x6000..0x8000 => self.ram.watch_address(addr, kind),
            0x8000.. => self.rom.watch_address(addr, kind),
        }
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.driver
            .watch_label(label, kind)
            .or_else(|| self.ram.watch_label(label, kind))
            .or_else(|| self.rom.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.driver.unwatch(id);
        self.ram.unwatch(id);
        self.rom.unwatch(id);
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        match addr & 0xffff {
            0x0000..0x6000 => self.driver.peek_byte(addr),
            0x6000..0x8000 => self.ram.peek_byte(addr),
            0x8000.. => self.rom.peek_byte(addr),
        }
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        match addr & 0xffff {
            0x0000..0x6000 => self.driver.read_byte(addr),
            0x6000..0x8000 => self.ram.read_byte(addr),
            0x8000.. => self.rom.read_byte(addr),
        }
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        match addr & 0xffff {
            0x0000..0x6000 => self.driver.write_byte(addr, data),
            0x6000..0x8000 => self.ram.write_byte(addr, data),
            0x8000.. => self.rom.write_byte(addr, data),
        }
    }
}

//===========================================================================//
