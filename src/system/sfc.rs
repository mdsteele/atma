use super::sim::SimSystem;
use super::util::{
    new_rom_bus_with_binary_symbols, with_offset_free_symbols, with_symbols,
};
use crate::bus::{
    SimBus, new_ram_bus, new_snes_cpu_bus, new_snes_lorom_cart_bus,
    new_snes_ssmp_bus,
};
use crate::parse::SymbolTable;
use crate::proc::{SimProc, Spc700, Wdc65c816};
use std::io::{self, Read, Seek, SeekFrom};
use std::rc::Rc;

//===========================================================================//

// Constants used for processer/address space names.
const CPU: &str = "CPU";
const SMP: &str = "SMP";

//===========================================================================//

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SnesMappingMode {
    LoRom,
    HiRom,
    ExHiRom,
}

impl SnesMappingMode {
    pub const ALL: &[Self] = &[Self::LoRom, Self::HiRom, Self::ExHiRom];

    pub fn from_mode_byte(byte: u8) -> Option<Self> {
        match byte & 0x0f {
            0x0 => Some(Self::LoRom),
            0x1 => Some(Self::HiRom),
            0x5 => Some(Self::ExHiRom),
            _ => None,
        }
    }

    pub fn header_rom_address(self) -> usize {
        // See https://snes.nesdev.org/wiki/ROM_file_formats and
        // https://www.youtube.com/watch?v=-U76YvWdnZM
        match self {
            Self::LoRom => 0x007fc0,
            Self::HiRom => 0x00ffc0,
            Self::ExHiRom => 0x40ffc0,
        }
    }

    pub fn max_rom_size(self) -> usize {
        // See https://snes.nesdev.org/wiki/Memory_map and
        // https://www.youtube.com/watch?v=-U76YvWdnZM
        match self {
            Self::LoRom => 0x400000,   // 4 MiB
            Self::HiRom => 0x400000,   // 4 MiB
            Self::ExHiRom => 0x800000, // 8 MiB
        }
    }
}

//===========================================================================//

struct SnesRomHeader {
    mode: SnesMappingMode,
    sram_size: usize,
    metadata: Vec<(Rc<str>, Rc<str>)>,
}

impl SnesRomHeader {
    pub fn detect(rom_data: &[u8]) -> io::Result<SnesRomHeader> {
        let rom_size = rom_data.len();
        for &mode in SnesMappingMode::ALL {
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

            let title = &header[0..0x15];
            let chipset_byte = header[0x16];
            let is_superfx = chipset_byte & 0xf0 == 0x10;
            let base_sram_shift = header[0x18];
            let country_code = header[0x19];
            let dev_id_byte = header[0x1a];
            let rom_version = header[0x1b];

            let expanded_header = &rom_data[(header_addr - 0x10)..][..0x10];
            let has_expanded_header = dev_id_byte == 0x33;
            let chipset_subtype = if has_expanded_header {
                Some(expanded_header[15])
            } else {
                None
            };

            let mut metadata = vec![
                title_metadata(title),
                mode_metadata(mode_byte),
                chipset_metadata(chipset_byte, chipset_subtype),
                country_metadata(country_code),
            ];

            let sram_shift = if has_expanded_header {
                metadata.push(maker_code_metadata(&expanded_header[0..2]));
                metadata.push(game_code_metadata(&expanded_header[2..6]));
                // SuperFX games with an expanded ROM header encode the amount
                // of onboard cart RAM in the "Expansion RAM Size" field of the
                // expanded header instead of in the normal "RAM size" field in
                // the standard header, for some reason.  For details see
                // https://sneslab.net/wiki/SNES_ROM_Header.
                if is_superfx { expanded_header[0xd] } else { base_sram_shift }
            } else {
                metadata.push(dev_id_metadata(dev_id_byte));
                if is_superfx {
                    // SuperFX games that are too old to use the expanded
                    // header (e.g. Star Fox) still have 32kB of cart RAM for
                    // use by the SuperFX, even though the RAM size field in
                    // the ROM header is set to zero.
                    5
                } else {
                    base_sram_shift
                }
            };
            let sram_size =
                if sram_shift == 0 { 0 } else { 1024 << sram_shift };
            metadata.push(rom_version_metadata(rom_version));
            return Ok(SnesRomHeader { mode, sram_size, metadata });
        }
        Err(io::Error::other("could not find any SNES ROM header"))
    }

    pub fn make_cpu_bus(
        &self,
        rom_data: Box<[u8]>,
        rom_start: u64,
        symbols: &SymbolTable,
    ) -> Box<dyn SimBus> {
        let rom_bus =
            new_rom_bus_with_binary_symbols(rom_data, rom_start, CPU, symbols);
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
        with_offset_free_symbols(new_snes_cpu_bus(cart_bus), CPU, symbols)
    }
}

//===========================================================================//

/// Reads a compiled SFC (or SMC) binary file, and returns a simulated system
/// that represents it.
pub fn load_sfc_binary<R: Read + Seek>(
    symbols: SymbolTable,
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

    let cpu_bus =
        header.make_cpu_bus(rom_data.into_boxed_slice(), rom_start, &symbols);
    let cpu_proc: Box<dyn SimProc> = Box::new(Wdc65c816::new());
    let smp_bus = with_symbols(
        new_snes_ssmp_bus(new_ram_bus(vec![0u8; 1 << 16].into_boxed_slice())),
        SMP,
        &symbols,
    );
    let smp_proc: Box<dyn SimProc> = Box::new(Spc700::new());
    let processors = vec![
        (Rc::from(CPU), (cpu_proc, cpu_bus)),
        (Rc::from(SMP), (smp_proc, smp_bus)),
    ];
    Ok(SimSystem::new(processors, header.metadata))
}

//===========================================================================//

fn chipset_metadata(
    chipset: u8,
    opt_subtype: Option<u8>,
) -> (Rc<str>, Rc<str>) {
    // These mappings come from https://snes.nesdev.org/wiki/ROM_header and
    // from fullsnes section "SNES Cartridge ROM Header".
    let coprocessor = match chipset & 0xf0 {
        0x00 => "DSP",
        0x10 => "SuperFX GSU",
        0x20 => "OBC1",
        0x30 => "SA-1",
        0x40 => "S-DD1",
        0x50 => "S-RTC",
        0xe0 => "Super Game Boy/Satellaview",
        0xf0 => match opt_subtype {
            Some(0x00) => "SPC7110",
            Some(0x01) => "ST010/ST011",
            Some(0x02) => "ST018",
            Some(0x10) => "CX4",
            _ => "unknown coprocessor",
        },
        _ => "unknown coprocessor",
    };
    let description = match chipset & 0x0f {
        0x00 => "ROM only".to_string(),
        0x01 => "ROM + RAM".to_string(),
        0x02 => "ROM + RAM + battery".to_string(),
        0x03 => format!("ROM + {coprocessor}"),
        0x04 => format!("ROM + {coprocessor} + RAM"),
        0x05 => format!("ROM + {coprocessor} + RAM + battery"),
        0x06 => format!("ROM + {coprocessor} + battery"),
        0x0a => format!("ROM + fast mode {coprocessor} + RAM + battery"),
        _ => "unknown".to_string(),
    };
    let subtype = if chipset & 0xf0 == 0xf0
        && let Some(subtype) = opt_subtype
    {
        format!("/${subtype:02x}")
    } else {
        "".to_string()
    };
    let key = Rc::from("Chipset");
    let value = Rc::from(format!("${chipset:02x}{subtype} ({description})"));
    (key, value)
}

fn country_metadata(country_code: u8) -> (Rc<str>, Rc<str>) {
    // These mappings come from https://snes.nesdev.org/wiki/ROM_header
    let country_name = match country_code {
        0x00 => "Japan",
        0x01 => "North America",
        0x02 => "Europe",
        0x03 => "Scandinavia",
        0x04 => "Finland",
        0x05 => "Denmark",
        0x06 => "French",
        0x07 => "Dutch",
        0x08 => "Spanish",
        0x09 => "German",
        0x0A => "Italian",
        0x0B => "Chinese",
        0x0C => "Indonesia",
        0x0D => "South Korea",
        0x0E => "Common",
        0x0F => "Canada",
        0x10 => "Brazil/NGS",
        0x11 => "Australia",
        _ => "unknown",
    };
    let key = Rc::from("Country");
    let value = Rc::from(format!("${country_code:02x} ({country_name})"));
    (key, value)
}

fn dev_id_metadata(dev_id_byte: u8) -> (Rc<str>, Rc<str>) {
    (Rc::from("Developer ID"), Rc::from(format!("${dev_id_byte:02x}")))
}

fn game_code_metadata(game_code: &[u8]) -> (Rc<str>, Rc<str>) {
    (Rc::from("Game Code"), string_field(game_code))
}

fn maker_code_metadata(maker_code: &[u8]) -> (Rc<str>, Rc<str>) {
    (Rc::from("Maker Code"), string_field(maker_code))
}

fn mode_metadata(mode_byte: u8) -> (Rc<str>, Rc<str>) {
    // These mappings come from https://snes.nesdev.org/wiki/ROM_header
    let speed = if mode_byte & 0x10 == 0 { "slow" } else { "fast" };
    let mode = match mode_byte & 0x0f {
        0x0 => "LoROM",
        0x1 => "HiROM",
        0x2 => "S-DD1",
        0x3 => "SA-1",
        0x5 => "ExHiROM",
        0xa => "SPC7110",
        _ => "unknown",
    };
    (Rc::from("Mode"), Rc::from(format!("${mode_byte:02x} ({speed} {mode})")))
}

fn rom_version_metadata(rom_version: u8) -> (Rc<str>, Rc<str>) {
    (Rc::from("ROM Version"), Rc::from(format!("1.{rom_version}")))
}

fn title_metadata(title: &[u8]) -> (Rc<str>, Rc<str>) {
    (Rc::from("Title"), string_field(title))
}

//===========================================================================//

fn string_field(bytes: &[u8]) -> Rc<str> {
    Rc::from(String::from_utf8_lossy(bytes).trim_end_matches('\0').trim())
}

//===========================================================================//
