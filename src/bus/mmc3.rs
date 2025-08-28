use super::SimBus;

//===========================================================================//

const PROTECT_FLAG_ENABLE_RAM: u8 = 0b1000_0000;
const PROTECT_FLAG_DENY_WRITE: u8 = 0b0100_0000;

const SELECT_FLAG_8000_FIXED: u8 = 0b0100_0000;

//===========================================================================//

/// A simulated memory bus for an NES cartridge with an MMC3 mapper chip.
pub struct Mmc3Bus {
    ram: Box<dyn SimBus>,
    rom: Box<dyn SimBus>,
    bank_select: u8,
    bank_registers: [u8; 8],
    ram_protect: u8,
}

impl Mmc3Bus {
    /// Constructs a simulated MMC3 cartridge memory bus, given the busses for
    /// its RAM and ROM chips.
    pub fn new(ram: Box<dyn SimBus>, rom: Box<dyn SimBus>) -> Mmc3Bus {
        Mmc3Bus {
            ram,
            rom,
            bank_select: 0,
            bank_registers: [0; 8],
            ram_protect: 0,
        }
    }

    fn translate_rom_address(&self, addr: u32) -> u32 {
        let bank = match addr & 0x7fff {
            0x0000..0x2000 => {
                if (self.bank_select & SELECT_FLAG_8000_FIXED) != 0 {
                    0xfe
                } else {
                    self.bank_registers[6]
                }
            }
            0x2000..0x4000 => self.bank_registers[7],
            0x4000..0x6000 => {
                if (self.bank_select & SELECT_FLAG_8000_FIXED) != 0 {
                    self.bank_registers[6]
                } else {
                    0xfe
                }
            }
            0x6000.. => 0xff,
        };
        (u32::from(bank) << 13) | (addr & 0x1fff)
    }
}

impl SimBus for Mmc3Bus {
    fn description(&self) -> String {
        format!(
            "MMC3 with {} and {}",
            self.ram.description(),
            self.rom.description()
        )
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match addr & 0xffff {
            0x0000..0x6000 => None,
            0x6000..0x8000 => self.ram.label_at(addr),
            0x8000.. => self.rom.label_at(self.translate_rom_address(addr)),
        }
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        match addr & 0xffff {
            0x0000..0x6000 => 0,
            0x6000..0x8000 => self.ram.peek_byte(addr),
            0x8000.. => self.rom.peek_byte(self.translate_rom_address(addr)),
        }
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        match addr & 0xffff {
            0x0000..0x6000 => 0,
            0x6000..0x8000 => {
                if (self.ram_protect & PROTECT_FLAG_ENABLE_RAM) != 0 {
                    self.ram.read_byte(addr)
                } else {
                    0
                }
            }
            0x8000.. => self.rom.read_byte(self.translate_rom_address(addr)),
        }
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        match addr & 0xffff {
            0x0000..0x6000 => {}
            0x6000..0x8000 => {
                if (self.ram_protect & PROTECT_FLAG_ENABLE_RAM) != 0
                    && (self.ram_protect & PROTECT_FLAG_DENY_WRITE) == 0
                {
                    self.ram.write_byte(addr, data);
                }
            }
            0x8000..0xA000 => {
                if (addr & 1) != 0 {
                    let index = usize::from(self.bank_select & 0b111);
                    self.bank_registers[index] = data;
                } else {
                    self.bank_select = data;
                }
            }
            0xA000..0xC000 => {
                if (addr & 1) != 0 {
                    self.ram_protect = data;
                } else {
                    // This should set nametable mirroring, but we don't
                    // currently simulate that.
                }
            }
            0xC000.. => {
                // This should affect HBlank IRQ settings, but we don't
                // currently simulate those.
            }
        }
    }
}

//===========================================================================//
