use super::sim::SimSystem;
use crate::bus::{
    Mmc3Bus, NesBus, SimBus, new_open_bus, new_ram_bus, new_rom_bus,
};
use crate::proc::{Mos6502, SimProc};
use std::io::{self, Read, Seek};
use std::rc::Rc;

//===========================================================================//

/// NES mappers that are currently supported by this crate.
enum NesMapper {
    Nrom,
    Mmc3,
}

impl NesMapper {
    fn from_mapper_number(mapper_number: u16) -> Option<NesMapper> {
        match mapper_number {
            0 => Some(NesMapper::Nrom),
            4 => Some(NesMapper::Mmc3),
            _ => None,
        }
    }

    fn make_cpu_bus(
        &self,
        sram_size: usize,
        rom: Box<[u8]>,
    ) -> Box<dyn SimBus> {
        let ram_bus = if sram_size == 0 {
            new_open_bus(16)
        } else {
            new_ram_bus(vec![0u8; sram_size].into_boxed_slice())
        };
        let rom_bus: Box<dyn SimBus> = new_rom_bus(rom);
        let cart = match *self {
            NesMapper::Nrom => rom_bus,
            NesMapper::Mmc3 => Box::new(Mmc3Bus::new(ram_bus, rom_bus)),
        };
        Box::new(NesBus::with_cartridge(cart))
    }
}

//===========================================================================//

/// Reads a compiled iNES or NES 2.0 binary file, and returns a simulated
/// system that represents it.
pub fn load_nes_binary<R: Read + Seek>(
    mut reader: R,
) -> io::Result<SimSystem> {
    let mut header = [0u8; 16];
    reader.read_exact(&mut header)?;
    if &header[..4] != b"NES\x1a" {
        let message = "incorrect magic number in iNES header";
        return Err(io::Error::other(message));
    }
    let version = (header[7] >> 2) & 0x3;

    let has_trainer = (header[6] & 0x04) != 0;
    if has_trainer {
        let message = "iNES trainers are not yet supported";
        return Err(io::Error::other(message));
    }

    let mapper_number: u16 = {
        let lo_nibble = (header[6] >> 4) as u16;
        let md_nibble = (header[7] >> 4) as u16;
        let hi_nibble =
            if version == 2 { (header[8] & 0x0f) as u16 } else { 0 };
        (hi_nibble << 8) | (md_nibble << 4) | lo_nibble
    };
    let mapper = match NesMapper::from_mapper_number(mapper_number) {
        Some(mapper) => mapper,
        None => {
            let message =
                format!("unsupported mapper number: {}", mapper_number);
            return Err(io::Error::other(message));
        }
    };

    let prg_rom_size: usize = {
        let lo_byte = header[4];
        let hi_byte = if version == 2 { header[4] & 0x0f } else { 0 };
        0x4000 * (((hi_byte as usize) << 8) | (lo_byte as usize))
    };

    let sram_size: usize = 0x40 << (header[10] >> 4);

    let mut prg_rom = vec![0u8; prg_rom_size];
    reader.read_exact(&mut prg_rom)?;

    let bus = mapper.make_cpu_bus(sram_size, prg_rom.into_boxed_slice());
    let cpu: Box<dyn SimProc> = Box::new(Mos6502::new());
    let processors = vec![(Rc::from("cpu"), (cpu, bus))];
    Ok(SimSystem::new(processors))
}

//===========================================================================//
