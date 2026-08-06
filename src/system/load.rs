use super::cc65::load_sim65_binary;
use super::gb::{GB_HEADER_LOGO, load_gb_binary};
use super::gbs::load_gbs_binary;
use super::nes::load_nes_binary;
use super::nsf::load_nsf_binary;
use super::sfc::load_sfc_binary;
use super::sim::SimSystem;
use super::spc::load_spc_binary;
use std::io::{self, Read, Seek, SeekFrom};

//===========================================================================//

/// A format of compiled binary that can be loaded into a simulated system by
/// [`load_binary_with_format`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryFormat {
    /// Detect the format automatically.
    Auto,
    /// A GB/GBC file (Game Boy cartridge).
    Gb,
    /// A GBS file (Game Boy audio dump).
    Gbs,
    /// An iNES or NES 2.0 file (NES cartridge).
    Nes,
    /// An NSF file (NES audio dump).
    Nsf,
    /// An SFC/SMC file (SNES cartridge).
    Sfc,
    /// A cc65 sim65 binary.
    Sim65,
    /// An SPC file (SNES audio dump).
    Spc,
}

//===========================================================================//

/// Reads a compiled binary file in the specified format, and returns a
/// simulated system that represents it.
pub fn load_binary_with_format<R: Read + Seek>(
    format: BinaryFormat,
    reader: R,
) -> io::Result<SimSystem> {
    match format {
        BinaryFormat::Auto => load_binary(reader),
        BinaryFormat::Gb => load_gb_binary(reader),
        BinaryFormat::Gbs => load_gbs_binary(reader),
        BinaryFormat::Nes => load_nes_binary(reader),
        BinaryFormat::Nsf => load_nsf_binary(reader),
        BinaryFormat::Sfc => load_sfc_binary(reader),
        BinaryFormat::Sim65 => load_sim65_binary(reader),
        BinaryFormat::Spc => load_spc_binary(reader),
    }
}

//===========================================================================//

/// Reads a compiled binary file, and returns a simulated system that
/// represents it.
///
/// The format of the binary is detected automatically.  The following binary
/// formats are currently supported:
/// * GB/GBC
/// * GBS
/// * iNES
/// * NES 2.0
/// * NSF
/// * sim65 (6502 mode only)
/// * SFC/SMC
/// * SPC
pub fn load_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimSystem> {
    let mut header = [0u8; 8];
    reader.read_exact(&mut header)?;
    if &header[..4] == b"GBS\x01" {
        reader.rewind()?;
        return load_gbs_binary(reader);
    }
    if &header[..4] == b"NES\x1a" {
        reader.rewind()?;
        return load_nes_binary(reader);
    }
    if &header[..5] == b"NESM\x1a" {
        reader.rewind()?;
        return load_nsf_binary(reader);
    }
    if &header == b"SNES-SPC" {
        reader.rewind()?;
        return load_spc_binary(reader);
    }
    if &header[..5] == b"sim65" {
        reader.rewind()?;
        return load_sim65_binary(reader);
    }

    reader.seek(SeekFrom::Start(0x0104))?;
    let mut logo = [0u8; 0x30];
    reader.read_exact(&mut logo)?;
    if &logo == GB_HEADER_LOGO {
        reader.rewind()?;
        return load_gb_binary(reader);
    }

    reader.rewind()?;
    load_sfc_binary(reader)
}

//===========================================================================//
