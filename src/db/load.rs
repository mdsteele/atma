use super::env::SimEnv;
use crate::bus::{Mmc3Bus, NesBus, RamBus, RomBus, SimBus, null_bus};
use crate::proc::{Mos6502, SharpSm83, SimProc};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use std::io::{self, Read, Seek, SeekFrom, Write};

//===========================================================================//

macro_rules! invalid_data {
    ($e:expr) => {
        return Err(::std::io::Error::new(::std::io::ErrorKind::InvalidData,
                                         $e))
    };
    ($fmt:expr, $($arg:tt)+) => {
        return Err(::std::io::Error::new(::std::io::ErrorKind::InvalidData,
                                         format!($fmt, $($arg)+)))
    };
}

//===========================================================================//

const GB_HEADER_LOGO: &[u8; 0x30] = &[
    0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b, 0x03, 0x73, 0x00, 0x83,
    0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e,
    0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99, 0xbb, 0xbb, 0x67, 0x63,
    0x6e, 0x0e, 0xec, 0xcc, 0xdd, 0xdc, 0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e,
];

//===========================================================================//

/// Reads a compiled binary file, and returns a simulated environment that
/// represents it.
///
/// The following binary formats are currently supported:
/// * GB/GBC
/// * sim65 (6502 mode only)
///
/// The following binary formats will be supported in the future:
/// * iNES
/// * NES 2.0
/// * sim65 (65C02 mode)
/// * SFC/SMC
/// * SPC
pub fn load_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
    let mut processors = Vec::<(String, Box<dyn SimProc>)>::new();
    let mut header = [0u8; 8];
    reader.read_exact(&mut header)?;
    if &header[..4] == b"NES\x1a" {
        reader.rewind()?;
        return load_nes_binary(reader);
    } else if &header[..5] == b"sim65" {
        // Read rest of sim65 header.
        let version = header[5];
        if version != 2 {
            invalid_data!("unsupported sim65 binary version: {}", version);
        }
        let cpu_type = header[6];
        if cpu_type != 0 {
            invalid_data!("unsupported sim65 cpu type: {}", cpu_type);
        }
        let load_addr = reader.read_u16::<LittleEndian>()?;
        let reset_addr = reader.read_u16::<LittleEndian>()?;
        // Read sim65 code payload.
        let mut code = Vec::<u8>::new();
        reader.read_to_end(&mut code)?;
        if load_addr as usize + code.len() > 0x10000 {
            invalid_data!("too much data");
        }
        // Load code into RAM at load_addr.
        let mut ram = Box::new([0u8; 0x10000]);
        let mut cursor = io::Cursor::new(ram.as_mut_slice());
        cursor.seek(SeekFrom::Start(load_addr.into()))?;
        cursor.write_all(&code)?;
        // Copy reset_addr into RAM at the reset vector.
        cursor.seek(SeekFrom::Start(0xfffc))?;
        cursor.write_u16::<LittleEndian>(reset_addr)?;
        let bus = Box::new(RamBus::new(ram));
        let cpu = Box::new(Mos6502::new(bus));
        processors.push(("cpu".to_string(), cpu));
    } else {
        reader.seek(SeekFrom::Start(0x0104))?;
        let mut logo = [0u8; 0x30];
        reader.read_exact(&mut logo)?;
        if &logo == GB_HEADER_LOGO {
            // TODO: Emulate memory bus based on mapper byte.
            let ram = Box::new([0u8; 0x10000]);
            let bus = Box::new(RamBus::new(ram));
            let cpu = Box::new(SharpSm83::new(bus));
            processors.push(("cpu".to_string(), cpu));
        } else {
            invalid_data!("unsupported binary file type");
        }
    }
    Ok(SimEnv::new(processors))
}

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
            null_bus()
        } else {
            Box::new(RamBus::new(vec![0u8; sram_size].into_boxed_slice()))
        };
        let rom_bus: Box<dyn SimBus> = Box::new(RomBus::new(rom));
        let cart = match *self {
            NesMapper::Nrom => rom_bus,
            NesMapper::Mmc3 => Box::new(Mmc3Bus::new(ram_bus, rom_bus)),
        };
        Box::new(NesBus::with_cartridge(cart))
    }
}

pub fn load_nes_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
    let mut header = [0u8; 16];
    reader.read_exact(&mut header)?;
    if &header[..4] != b"NES\x1a" {
        invalid_data!("incorrect magic number in iNES header");
    }
    let version = (header[7] >> 2) & 0x3;

    let has_trainer = (header[6] & 0x04) != 0;
    if has_trainer {
        invalid_data!("iNES trainers are not yet supported");
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
        None => invalid_data!("unsupported mapper number: {}", mapper_number),
    };

    let prg_rom_size: usize = {
        let lo_byte = header[4];
        let hi_byte = if version == 2 { header[4] & 0x0f } else { 0 };
        0x4000 * (((hi_byte as usize) << 8) | (lo_byte as usize))
    };

    let sram_size: usize = 0x40 << (header[10] >> 4);

    let mut prg_rom = vec![0u8; prg_rom_size];
    reader.read_exact(&mut prg_rom)?;

    let cpu_bus = mapper.make_cpu_bus(sram_size, prg_rom.into_boxed_slice());
    let cpu: Box<dyn SimProc> = Box::new(Mos6502::new(cpu_bus));
    let processors = vec![("cpu".to_string(), cpu)];
    Ok(SimEnv::new(processors))
}

//===========================================================================//
