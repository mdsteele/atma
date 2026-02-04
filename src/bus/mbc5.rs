use super::{SimBus, WatchId, WatchKind};
use crate::addr::Addr;

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

    fn translate_rom_address(&self, addr: Addr) -> Addr {
        let bank = match addr.as_u16() & 0x7fff {
            0x0000..0x4000 => 0,
            0x4000.. => self.rom_bank,
        };
        (Addr::from(bank) << 14) | (addr & Addr::from(0x3fffu16))
    }

    fn translate_ram_address(&self, addr: Addr) -> Addr {
        (Addr::from(self.ram_bank) << 13) | (addr & Addr::from(0x1fffu16))
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

    fn label_at(&self, addr: Addr) -> Option<&str> {
        match addr.as_u16() {
            0x0000..0x8000 => {
                self.rom.label_at(self.translate_rom_address(addr))
            }
            0x8000.. => self.ram.label_at(self.translate_ram_address(addr)),
        }
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        match addr.as_u16() {
            0x0000..0x8000 => {
                self.rom.watchpoint_at(self.translate_rom_address(addr), kind)
            }
            0x8000.. => {
                self.ram.watchpoint_at(self.translate_ram_address(addr), kind)
            }
        }
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        match addr.as_u16() {
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

    fn peek_byte(&self, addr: Addr) -> u8 {
        match addr.as_u16() {
            0x0000..0x8000 => {
                self.rom.peek_byte(self.translate_rom_address(addr))
            }
            0x8000.. => {
                // TODO: emulate RAM enable
                self.ram.peek_byte(self.translate_ram_address(addr))
            }
        }
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        match addr.as_u16() {
            0x0000..0x8000 => {
                self.rom.read_byte(self.translate_rom_address(addr))
            }
            0x8000.. => {
                // TODO: emulate RAM enable
                self.ram.read_byte(self.translate_ram_address(addr))
            }
        }
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        match addr.as_u16() {
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
