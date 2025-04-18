use super::bus::Ram64k;
use super::env::SimEnv;
use super::proc::{Mos6502, SharpSm83, SimProc};
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
        invalid_data!("iNES binaries are not yet supported");
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
        let bus = Box::new(Ram64k::new(ram));
        let cpu = Box::new(Mos6502::new(bus));
        processors.push(("cpu".to_string(), cpu));
    } else {
        reader.seek(SeekFrom::Start(0x0104))?;
        let mut logo = [0u8; 0x30];
        reader.read_exact(&mut logo)?;
        if &logo == GB_HEADER_LOGO {
            // TODO: Emulate memory bus based on mapper byte.
            let ram = Box::new([0u8; 0x10000]);
            let bus = Box::new(Ram64k::new(ram));
            let cpu = Box::new(SharpSm83::new(bus));
            processors.push(("cpu".to_string(), cpu));
        } else {
            invalid_data!("unsupported binary file type");
        }
    }
    return Ok(SimEnv::new(processors));
}

//===========================================================================//
