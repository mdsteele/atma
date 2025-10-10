use super::env::SimEnv;
use crate::bus::{
    DmgBus, Mbc5Bus, Mmc3Bus, NesBus, SimBus, new_nsf_bus, new_open_bus,
    new_ram_bus, new_rom_bus,
};
use crate::proc::{Mos6502, SharpSm83, SimProc, Spc700};
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
/// * GBS
/// * iNES
/// * NES 2.0
/// * NSF
/// * sim65 (6502 mode only)
/// * SPC
///
/// The following binary formats will be supported in the future:
/// * NSF2
/// * NSFe
/// * SFC/SMC
pub fn load_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
    let mut processors =
        Vec::<(String, (Box<dyn SimProc>, Box<dyn SimBus>))>::new();
    let mut header = [0u8; 8];
    reader.read_exact(&mut header)?;
    if &header[..4] == b"GBS\x01" {
        reader.rewind()?;
        return load_gbs_binary(reader);
    } else if &header[..4] == b"NES\x1a" {
        reader.rewind()?;
        return load_nes_binary(reader);
    } else if &header[..5] == b"NESM\x1a" {
        reader.rewind()?;
        return load_nsf_binary(reader);
    } else if &header == b"SNES-SPC" {
        reader.rewind()?;
        return load_spc_binary(reader);
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
        let bus = new_ram_bus(ram);
        let cpu = Box::new(Mos6502::new());
        processors.push(("cpu".to_string(), (cpu, bus)));
    } else {
        reader.seek(SeekFrom::Start(0x0104))?;
        let mut logo = [0u8; 0x30];
        reader.read_exact(&mut logo)?;
        if &logo == GB_HEADER_LOGO {
            reader.rewind()?;
            return load_gb_binary(reader);
        } else {
            invalid_data!("unsupported binary file type");
        }
    }
    Ok(SimEnv::new(processors))
}

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
        let rom_bus: Box<dyn SimBus> = new_rom_bus(rom);
        let cart = match *self {
            GbMapper::RomOnly => rom_bus,
            GbMapper::Mbc5 => Box::new(Mbc5Bus::new(ram_bus, rom_bus)),
        };
        Box::new(DmgBus::with_cartridge(cart))
    }
}

fn load_gb_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
    // See https://gbdev.io/pandocs/The_Cartridge_Header.html
    reader.seek(SeekFrom::Start(0x0147))?;
    let mut metadata = [0u8; 3];
    reader.read_exact(&mut metadata)?;

    let cart_type_byte = metadata[0]; // address 0x0147
    let mapper = match GbMapper::from_cart_type(cart_type_byte) {
        Some(mapper) => mapper,
        None => invalid_data!("unsupported cart type: 0x{cart_type_byte:02x}"),
    };

    let rom_size_byte = metadata[1]; // address 0x0148
    let rom_size: usize = if rom_size_byte <= 0x08 {
        (1 << 15) << rom_size_byte
    } else {
        invalid_data!("invalid GB ROM size byte: 0x{rom_size_byte:02x}");
    };

    let ram_size_byte = metadata[2]; // address 0x0149
    let ram_size: usize = match ram_size_byte {
        0 => 0,
        2 => 1 << 13,
        3 => 1 << 15,
        4 => 1 << 17,
        5 => 1 << 16,
        _ => invalid_data!("invalid GB RAM size byte: 0x{ram_size_byte:02x}"),
    };

    // TODO: Emulate memory bus based on cart_type_byte.
    let mut rom_data = vec![0u8; rom_size];
    reader.rewind()?;
    reader.read_exact(&mut rom_data)?;
    let bus = mapper.make_cpu_bus(ram_size, rom_data.into_boxed_slice());
    let cpu: Box<dyn SimProc> = Box::new(SharpSm83::new());
    let processors = vec![("cpu".to_string(), (cpu, bus))];
    Ok(SimEnv::new(processors))
}

//===========================================================================//

fn load_gbs_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
    // See https://ocremix.org/info/GBS_Format_Specification
    let mut header = [0u8; 0x70];
    reader.read_exact(&mut header)?;

    if &header[0..3] != b"GBS" {
        invalid_data!("incorrect magic number for a GBS file");
    }

    let version = header[0x03];
    if version != 1 {
        invalid_data!("GBS version {version} is not supported");
    }

    let total_songs = header[0x04];
    if total_songs == 0 {
        invalid_data!("cannot load GBS with 0 total songs");
    }

    let starting_song = header[0x05];
    if !(1..=total_songs).contains(&starting_song) {
        invalid_data!(
            "invalid GBS starting song {starting_song} \
             (out of {total_songs} total songs)"
        );
    }

    let load_addr = (u16::from(header[0x07]) << 8) | u16::from(header[0x06]);
    if !(0x400..0x8000).contains(&load_addr) {
        invalid_data!("invalid GBS load address: ${load_addr:04x}");
    }
    let init_addr = (u16::from(header[0x09]) << 8) | u16::from(header[0x08]);
    if !(0x400..0x8000).contains(&init_addr) {
        invalid_data!("invalid GBS init address: ${init_addr:04x}");
    }
    let play_addr = (u16::from(header[0x0b]) << 8) | u16::from(header[0x0a]);
    if !(0x400..0x8000).contains(&play_addr) {
        invalid_data!("invalid GBS play address: ${play_addr:04x}");
    }
    let stack_pointer =
        (u16::from(header[0x0d]) << 8) | u16::from(header[0x0c]);

    let mut load_data = Vec::<u8>::new();
    reader.read_to_end(&mut load_data)?;
    let rom_size =
        (usize::from(load_addr) + load_data.len()).next_power_of_two();
    let num_banks = rom_size.div_ceil(0x4000);
    if num_banks > 256 {
        invalid_data!("too much GBS ROM data ({num_banks} 16k banks)");
    }

    let mut rom_data = vec![0u8; rom_size];
    // Set up RST handlers.
    for rst_index in 0..8 {
        let rst_addr: u16 = rst_index * 8;
        let offset = usize::from(rst_addr);
        let destination = load_addr + rst_addr;
        rom_data[offset] = 0xc3; // JP (unconditional) opcode
        rom_data[offset + 1] = destination as u8;
        rom_data[offset + 2] = (destination >> 8) as u8;
    }
    // Copy driver code starting at address 0x0100.
    rom_data[0x0100..0x0108].copy_from_slice(&[
        0xcd, // CALL (unconditional) opcode
        init_addr as u8,
        (init_addr >> 8) as u8,
        0xcd, // CALL (unconditional) opcode
        play_addr as u8,
        (play_addr >> 8) as u8,
        0x18, // JR (unconditional) opcode
        0xfb, // -5
    ]);
    // Copy GBS ROM data.
    rom_data[usize::from(load_addr)..][..load_data.len()]
        .copy_from_slice(&load_data);

    let ram_bus = new_ram_bus(vec![0u8; 0x2000].into_boxed_slice());
    let rom_bus = new_rom_bus(rom_data.into_boxed_slice());
    let cart_bus = Box::new(Mbc5Bus::new(ram_bus, rom_bus));
    let bus: Box<dyn SimBus> = Box::new(DmgBus::with_cartridge(cart_bus));
    let mut cpu: Box<dyn SimProc> = Box::new(SharpSm83::new());
    cpu.set_register("A", u32::from(starting_song - 1));
    cpu.set_register("SP", u32::from(stack_pointer));
    let processors = vec![("cpu".to_string(), (cpu, bus))];
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

fn load_nes_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
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

    let bus = mapper.make_cpu_bus(sram_size, prg_rom.into_boxed_slice());
    let cpu: Box<dyn SimProc> = Box::new(Mos6502::new());
    let processors = vec![("cpu".to_string(), (cpu, bus))];
    Ok(SimEnv::new(processors))
}

//===========================================================================//

fn load_nsf_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
    // See https://www.nesdev.org/wiki/NSF
    let mut header = [0u8; 0x80];
    reader.read_exact(&mut header)?;

    let version = header[0x05];
    if version != 1 {
        invalid_data!("NSF version {version} is not supported");
    }

    let total_songs = header[0x06];
    if total_songs == 0 {
        invalid_data!("cannot load NSF with 0 total songs");
    }

    let starting_song = header[0x07];
    if !(1..=total_songs).contains(&starting_song) {
        invalid_data!(
            "invalid NSF starting song {starting_song} \
             (out of {total_songs} total songs)"
        );
    }

    let load_addr = (u16::from(header[0x09]) << 8) | u16::from(header[0x08]);
    if load_addr < 0x8000 {
        invalid_data!("invalid NSF load address: ${load_addr:04x}");
    }
    let init_addr = (u16::from(header[0x0b]) << 8) | u16::from(header[0x0a]);
    if init_addr < 0x8000 {
        invalid_data!("invalid NSF init address: ${init_addr:04x}");
    }
    let play_addr = (u16::from(header[0x0d]) << 8) | u16::from(header[0x0c]);
    if play_addr < 0x8000 {
        invalid_data!("invalid NSF play address: ${play_addr:04x}");
    }

    if header[0x70..0x78] != [0, 0, 0, 0, 0, 0, 0, 0] {
        invalid_data!("NSF bank switching is not yet supported");
    }

    let platform = header[0x7a];
    if (platform & 0x03) != 0 {
        invalid_data!("invalid NSF NTSC/PAL byte: ${platform:02x}");
    }

    let data_len = (usize::from(header[0x7f]) << 16)
        | (usize::from(header[0x7e]) << 8)
        | usize::from(header[0x7d]);
    let load_data: Vec<u8> = if data_len > 0 {
        let mut data = vec![0u8; data_len];
        reader.read_exact(&mut data)?;
        data
    } else {
        let mut data = Vec::<u8>::new();
        reader.read_to_end(&mut data)?;
        data
    };
    let data_len = load_data.len();

    let rom_data: Box<[u8]> = {
        let mut rom_data = vec![0u8; 0x8000];
        let start = (load_addr - 0x8000) as usize;
        let end = start + data_len;
        if end > rom_data.len() {
            invalid_data!(
                "cannot load ${data_len:x}-byte data payload at address \
                 ${load_addr:04x}"
            );
        }
        rom_data[start..end].copy_from_slice(&load_data);
        rom_data.into_boxed_slice()
    };

    let cart = new_nsf_bus(rom_data, init_addr, play_addr);
    let bus: Box<dyn SimBus> = Box::new(NesBus::with_cartridge(cart));
    let mut cpu: Box<dyn SimProc> = Box::new(Mos6502::new());
    cpu.set_pc(0x4800);
    cpu.set_register("A", u32::from(starting_song - 1));
    cpu.set_register("X", u32::from(platform & 0x01));
    cpu.set_register("S", 0xff);
    let processors = vec![("cpu".to_string(), (cpu, bus))];
    Ok(SimEnv::new(processors))
}

//===========================================================================//

fn load_spc_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
    // See https://wiki.superfamicom.org/spc-and-rsn-file-format
    let mut header = [0u8; 0x2e];
    reader.read_exact(&mut header)?;
    if &header[0..0x21] != b"SNES-SPC700 Sound File Data v0.30" {
        invalid_data!("incorrect format name in SPC header");
    }
    let minor_version = header[0x24];
    if minor_version != 30 {
        invalid_data!("unsupported SPC minor version: {minor_version}");
    }
    let id666_indicator = header[0x23];
    if id666_indicator != 26 {
        invalid_data!("unsupported ID666 indicator byte: {id666_indicator}");
    };

    let pc = (u16::from(header[0x26]) << 8) | u16::from(header[0x25]);
    let reg_a = header[0x27];
    let reg_x = header[0x28];
    let reg_y = header[0x29];
    let reg_psw = header[0x2a];
    let reg_sp = header[0x2b];

    reader.seek(SeekFrom::Start(0x100))?;
    let mut ram = vec![0u8; 1 << 16];
    reader.read_exact(&mut ram)?;

    // TODO: Use a specialized bus to support hardware registers and boot ROM.
    let bus = new_ram_bus(ram.into_boxed_slice());
    let mut cpu: Box<dyn SimProc> = Box::new(Spc700::new());
    cpu.set_pc(u32::from(pc));
    cpu.set_register("A", u32::from(reg_a));
    cpu.set_register("X", u32::from(reg_x));
    cpu.set_register("Y", u32::from(reg_y));
    cpu.set_register("PSW", u32::from(reg_psw));
    cpu.set_register("SP", u32::from(reg_sp));
    let processors = vec![("cpu".to_string(), (cpu, bus))];
    Ok(SimEnv::new(processors))
}

//===========================================================================//
