use super::sim::SimSystem;
use crate::bus::{
    SimBus, new_ram_bus, new_rom_bus, new_snes_cpu_bus,
    new_snes_lorom_cart_bus, new_snes_ssmp_bus,
};
use crate::proc::{SimProc, Spc700, Wdc65c816};
use std::io::{self, Read, Seek, SeekFrom};
use std::rc::Rc;

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SnesMappingMode {
    LoRom,
    HiRom,
    SuperMmc,
    Sas,
    SuperFx,
    ExHiRom,
    ExLoRom,
}

impl SnesMappingMode {
    fn from_mode_byte(byte: u8) -> Option<SnesMappingMode> {
        match byte & 0x0f {
            0x0 => Some(SnesMappingMode::LoRom),
            0x1 => Some(SnesMappingMode::HiRom),
            0x2 | 0xa => Some(SnesMappingMode::SuperMmc),
            0x3 => Some(SnesMappingMode::Sas),
            0x4 => Some(SnesMappingMode::SuperFx),
            0x5 => Some(SnesMappingMode::ExHiRom),
            0x6 => Some(SnesMappingMode::ExLoRom),
            _ => None,
        }
    }

    fn header_rom_address(self) -> usize {
        // See https://snes.nesdev.org/wiki/ROM_file_formats and
        // https://www.youtube.com/watch?v=-U76YvWdnZM
        match self {
            SnesMappingMode::LoRom => 0x007fc0,
            SnesMappingMode::HiRom => 0x00ffc0,
            SnesMappingMode::ExHiRom => 0x40ffc0,
            SnesMappingMode::ExLoRom => 0x407fc0,
            _ => todo!("{self:?}"),
        }
    }

    fn max_rom_size(self) -> usize {
        // See https://snes.nesdev.org/wiki/Memory_map and
        // https://www.youtube.com/watch?v=-U76YvWdnZM
        match self {
            SnesMappingMode::LoRom => 0x400000, // 4 MiB
            SnesMappingMode::HiRom => 0x400000, // 4 MiB
            SnesMappingMode::Sas => 0x800000,   // 8 MiB
            SnesMappingMode::SuperFx => 0x800000, // 8 MiB
            SnesMappingMode::ExHiRom => 0x800000, // 8 MiB
            SnesMappingMode::ExLoRom => 0x800000, // 8 MiB
            _ => todo!("{self:?}"),
        }
    }
}

//===========================================================================//

struct SnesRomHeader {
    title: String,
    mode: SnesMappingMode,
    sram_size: usize,
}

impl SnesRomHeader {
    pub fn detect(rom_data: &[u8]) -> io::Result<SnesRomHeader> {
        let rom_size = rom_data.len();
        for mode in [
            SnesMappingMode::LoRom,
            SnesMappingMode::HiRom,
            SnesMappingMode::ExHiRom,
            SnesMappingMode::ExLoRom,
        ] {
            let header_addr = mode.header_rom_address();
            if rom_size > mode.max_rom_size() || rom_size < header_addr + 0x20
            {
                continue;
            }
            let header = &rom_data[header_addr..][..0x20];
            let mode_byte = header[0x15];
            if SnesMappingMode::from_mode_byte(mode_byte) != Some(mode) {
                continue;
            }
            let checksum =
                (u16::from(header[0x1f]) << 8) | u16::from(header[0x1e]);
            let complement =
                (u16::from(header[0x1d]) << 8) | u16::from(header[0x1c]);
            if complement != !checksum {
                continue;
            }
            let chipset = header[0x16];
            let is_superfx = chipset & 0xf0 == 0x10;
            let base_sram_shift = header[0x18];
            let dev_id_byte = header[0x1a];
            let has_expanded_header = dev_id_byte == 0x33;
            let title = String::from_utf8_lossy(&header[0..0x15])
                .trim_end()
                .to_string();
            let sram_shift = if has_expanded_header {
                let expanded_header =
                    &rom_data[(header_addr - 0x10)..][..0x10];
                // SuperFX games with an expanded ROM header encode the amount
                // of onboard cart RAM in the "Expansion RAM Size" field of the
                // expanded header instead of in the normal "RAM size" field in
                // the standard header, for some reason.  For details see
                // https://sneslab.net/wiki/SNES_ROM_Header.
                if is_superfx { expanded_header[0xd] } else { base_sram_shift }
            } else if is_superfx {
                // SuperFX games that are too old to use the expanded header
                // (e.g. Star Fox) still have 32kB of cart RAM for use by the
                // SuperFX, even though the RAM size field in the ROM header is
                // set to zero.
                5
            } else {
                base_sram_shift
            };
            let sram_size =
                if sram_shift == 0 { 0 } else { 1024 << sram_shift };
            return Ok(SnesRomHeader { title, mode, sram_size });
        }
        Err(io::Error::other("could not find any SNES ROM header"))
    }

    pub fn make_cpu_bus(self, rom_data: Box<[u8]>) -> Box<dyn SimBus> {
        let rom_bus = new_rom_bus(rom_data);
        let sram_bus = if self.sram_size == 0 {
            None
        } else {
            Some(new_ram_bus(vec![0u8; self.sram_size].into_boxed_slice()))
        };
        let cart_bus: Box<dyn SimBus> = match self.mode {
            SnesMappingMode::LoRom => {
                new_snes_lorom_cart_bus(rom_bus, sram_bus)
            }
            SnesMappingMode::HiRom => rom_bus, // TODO: sram
            _ => todo!("{:?}", self.mode),
        };
        new_snes_cpu_bus(cart_bus)
    }
}

//===========================================================================//

/// Reads a compiled SFC (or SMC) binary file, and returns a simulated system
/// that represents it.
pub fn load_sfc_binary<R: Read + Seek>(
    mut reader: R,
) -> io::Result<SimSystem> {
    // From https://snes.nesdev.org/wiki/ROM_file_formats: "The data contained
    // in the file may be unheadered or headered...the headered version has 512
    // extra bytes at the start of the file...This extra data is generally
    // considered useless, except to the specific copier device that it was
    // originally used with...Because ROM files are generally expected to
    // include complete 32 or 64 kb banks, a simple way of detecting a header
    // is by checking if the file size modulo 1024 is equal to 512."
    let file_length = reader.seek(SeekFrom::End(0))?;
    let is_headered = match file_length % 1024 {
        0 => false,
        512 => true,
        _ => {
            let message =
                format!("unexpected SNES ROM file length: {file_length}");
            return Err(io::Error::other(message));
        }
    };
    let rom_start = if is_headered { 512 } else { 0 };
    let rom_size = file_length - rom_start;
    // According to https://forums.nesdev.org/viewtopic.php?t=5367, the largest
    // commercial ROM was 48 Mbit (6 MiB), and the largest fan-made ROM as of
    // 2009 was 96 Mbit (12 MiB).  For now, limit the ROM size to 16 MiB.
    if rom_size > 0x1000000 {
        let message = format!("SNES ROM is too large ({rom_size:#x} bytes)");
        return Err(io::Error::other(message));
    }
    let rom_size = rom_size as usize;

    reader.seek(SeekFrom::Start(rom_start))?;
    let mut rom_data = vec![0u8; rom_size.next_power_of_two()];
    reader.read_exact(&mut rom_data[..rom_size])?;

    let header = SnesRomHeader::detect(&rom_data)?;
    // TODO: Put these in SimSystem metadata instead of printing them here.
    println!("Title: {}", header.title);

    let cpu_bus = header.make_cpu_bus(rom_data.into_boxed_slice());
    let cpu_proc: Box<dyn SimProc> = Box::new(Wdc65c816::new());
    let apu_bus =
        new_snes_ssmp_bus(new_ram_bus(vec![0u8; 1 << 16].into_boxed_slice()));
    let apu_proc: Box<dyn SimProc> = Box::new(Spc700::new());
    let processors = vec![
        (Rc::from("cpu"), (cpu_proc, cpu_bus)),
        (Rc::from("apu"), (apu_proc, apu_bus)),
    ];
    Ok(SimSystem::new(processors))
}

//===========================================================================//
