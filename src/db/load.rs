use super::bus::Ram64k;
use super::env::SimEnv;
use super::proc::{Mos6502, SimProc};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use std::collections::HashMap;
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

/// Reads a compiled binary file, and returns a simulated environment that
/// represents it.
///
/// The following binary formats are currently supported:
/// * sim65 (6502 mode only)
///
/// The following binary formats will be supported in the future:
/// * GB/GBC
/// * iNES
/// * NES 2.0
/// * sim65 (65C02 mode)
/// * SFC/SMC
/// * SPC
pub fn load_binary<R: Read + Seek>(mut reader: R) -> io::Result<SimEnv> {
    let mut processors = HashMap::<String, Box<dyn SimProc>>::new();
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
        processors.insert("cpu".to_string(), cpu);
    } else {
        invalid_data!("unsupported binary file type");
    }
    return Ok(SimEnv { selected_processor: "cpu".to_string(), processors });
}

//===========================================================================//
