use super::{SimBus, WatchId, WatchKind};

//===========================================================================//

/// A simulated memory bus for a Game Boy cartridge with an MBC5 mapper chip.
pub struct Mbc5Bus {
    ram: Box<dyn SimBus>,
    rom: Box<dyn SimBus>,
    rom_bank: u16,
    ram_bank: u8,
}

impl Mbc5Bus {
    /// Constructs a simulated MBC5 cartridge memory bus, given the busses for
    /// its RAM and ROM chips.
    pub fn new(ram: Box<dyn SimBus>, rom: Box<dyn SimBus>) -> Mbc5Bus {
        Mbc5Bus { ram, rom, rom_bank: 1, ram_bank: 0 }
    }

    fn translate_rom_address(&self, addr: u32) -> u32 {
        let bank = match addr & 0x7fff {
            0x0000..0x4000 => 0,
            0x4000.. => self.rom_bank,
        };
        (u32::from(bank) << 14) | (addr & 0x3fff)
    }

    fn translate_ram_address(&self, addr: u32) -> u32 {
        (u32::from(self.ram_bank) << 13) | (addr & 0x1fff)
    }
}

impl SimBus for Mbc5Bus {
    fn description(&self) -> String {
        format!(
            "MBC5 with {} and {}",
            self.ram.description(),
            self.rom.description()
        )
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match addr as u16 {
            0x0000..0x8000 => {
                self.rom.label_at(self.translate_rom_address(addr))
            }
            0x8000.. => self.ram.label_at(self.translate_ram_address(addr)),
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        match addr as u16 {
            0x0000..0x8000 => {
                self.rom.watchpoint_at(self.translate_rom_address(addr), kind)
            }
            0x8000.. => {
                self.ram.watchpoint_at(self.translate_ram_address(addr), kind)
            }
        }
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        match addr as u16 {
            0x0000..0x8000 => {
                self.rom.watch_address(self.translate_rom_address(addr), kind)
            }
            0x8000.. => {
                self.ram.watch_address(self.translate_ram_address(addr), kind)
            }
        }
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.ram
            .watch_label(label, kind)
            .or_else(|| self.rom.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.ram.unwatch(id);
        self.rom.unwatch(id);
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        match addr as u16 {
            0x0000..0x8000 => {
                self.rom.peek_byte(self.translate_rom_address(addr))
            }
            0x8000.. => {
                // TODO: emulate RAM enable
                self.ram.peek_byte(self.translate_ram_address(addr))
            }
        }
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        match addr as u16 {
            0x0000..0x8000 => {
                self.rom.read_byte(self.translate_rom_address(addr))
            }
            0x8000.. => {
                // TODO: emulate RAM enable
                self.ram.read_byte(self.translate_ram_address(addr))
            }
        }
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        match addr as u16 {
            0x0000..0x2000 => {
                // TODO: set RAM enable
            }
            0x2000..0x4000 => {
                self.rom_bank &= 0x100;
                self.rom_bank |= u16::from(data);
            }
            0x4000..0x6000 => {
                self.rom_bank &= 0x0ff;
                self.rom_bank |= u16::from(data & 0x01) << 8;
            }
            0x6000..0x8000 => self.ram_bank = data & 0xf,
            0x8000.. => {
                // TODO: emulate RAM enable
                self.ram.write_byte(self.translate_ram_address(addr), data)
            }
        }
    }
}

//===========================================================================//
