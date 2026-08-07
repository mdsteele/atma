use super::sim::SimSystem;
use crate::bus::{
    SimBus, new_dmg_cpu_bus, new_gb_mbc5_cart_bus, new_open_bus, new_ram_bus,
    new_rom_bus,
};
use crate::proc::{SharpSm83, SimProc};
use std::io::{self, Read, Seek, SeekFrom};
use std::rc::Rc;

//===========================================================================//

pub(super) const GB_HEADER_LOGO: &[u8; 0x30] = &[
    0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b, 0x03, 0x73, 0x00, 0x83,
    0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e,
    0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99, 0xbb, 0xbb, 0x67, 0x63,
    0x6e, 0x0e, 0xec, 0xcc, 0xdd, 0xdc, 0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e,
];

//===========================================================================//

/// Game Boy mappers that are currently supported by this crate.
enum GbMapper {
    RomOnly,
    Mbc5,
}

impl GbMapper {
    fn from_cart_type(cart_type: u8) -> Option<GbMapper> {
        match cart_type {
            0x00 => Some(GbMapper::RomOnly),
            0x19..0x1f => Some(GbMapper::Mbc5),
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
        let rom_bus = new_rom_bus(rom);
        let cart_bus = match *self {
            GbMapper::RomOnly => rom_bus,
            GbMapper::Mbc5 => new_gb_mbc5_cart_bus(rom_bus, ram_bus),
        };
        new_dmg_cpu_bus(cart_bus)
    }
}

//===========================================================================//

/// Reads a compiled GB or GBC binary file, and returns a simulated system that
/// represents it.
pub fn load_gb_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimSystem> {
    // See https://gbdev.io/pandocs/The_Cartridge_Header.html
    reader.seek(SeekFrom::Start(0x0147))?;
    let mut metadata = [0u8; 3];
    reader.read_exact(&mut metadata)?;

    let cart_type_byte = metadata[0]; // address 0x0147
    let mapper = match GbMapper::from_cart_type(cart_type_byte) {
        Some(mapper) => mapper,
        None => {
            let message =
                format!("unsupported cart type: 0x{:02x}", cart_type_byte);
            return Err(io::Error::other(message));
        }
    };

    let rom_size_byte = metadata[1]; // address 0x0148
    let rom_size: usize = if rom_size_byte <= 0x08 {
        (1 << 15) << rom_size_byte
    } else {
        let message =
            format!("invalid GB ROM size byte: 0x{rom_size_byte:02x}");
        return Err(io::Error::other(message));
    };

    let ram_size_byte = metadata[2]; // address 0x0149
    let ram_size: usize = match ram_size_byte {
        0 => 0,
        2 => 1 << 13,
        3 => 1 << 15,
        4 => 1 << 17,
        5 => 1 << 16,
        _ => {
            let message =
                format!("invalid GB RAM size byte: 0x{ram_size_byte:02x}");
            return Err(io::Error::other(message));
        }
    };

    // TODO: Emulate memory bus based on cart_type_byte.
    let mut rom_data = vec![0u8; rom_size];
    reader.rewind()?;
    reader.read_exact(&mut rom_data)?;
    let bus = mapper.make_cpu_bus(ram_size, rom_data.into_boxed_slice());
    let cpu: Box<dyn SimProc> = Box::new(SharpSm83::new());
    let processors = vec![(Rc::from("cpu"), (cpu, bus))];
    Ok(SimSystem::new(processors))
}

//===========================================================================//
