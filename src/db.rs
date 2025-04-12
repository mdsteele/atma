//! Facilities for simulating and debugging compiled binaries.

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use std::collections::HashMap;
use std::io::{self, Read, Seek, SeekFrom, Write};

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

/// A simulated processor.
pub trait SimProc {
    /// Returns a human-readable description of this simulated processor.
    fn description(&self) -> String;

    /// Advances this processor by one instruction.
    fn step(&mut self);
}

/// A simulated MOS 6502 processor connected to 64k of RAM.
#[allow(dead_code)] // TODO
pub struct Mos6502 {
    pc: u16,
    reg_s: u8,
    reg_p: u8,
    reg_a: u8,
    reg_x: u8,
    reg_y: u8,
    ram: Box<[u8; 0x10000]>,
}

impl Mos6502 {
    /// Returns a new simulated MOS 6502 processor with the given RAM state.
    pub fn new(ram: Box<[u8; 0x10000]>) -> Mos6502 {
        let pc = ((ram[0xfffd] as u16) << 8) | (ram[0xfffc] as u16);
        Mos6502 {
            pc,
            reg_s: 0,
            reg_p: 0,
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            ram,
        }
    }
}

impl SimProc for Mos6502 {
    fn description(&self) -> String {
        format!("MOS 6502")
    }

    fn step(&mut self) {
        // TODO: implement this
    }
}

/// A complete simulated environment, including one or more processors and
/// memory address spaces.
pub struct SimEnv {
    selected_processor: String,
    processors: HashMap<String, Box<dyn SimProc>>,
}

impl SimEnv {
    /// Returns a human-readable, multi-line description of this simulated
    /// environment.
    pub fn description(&self) -> String {
        self.processors
            .iter()
            .map(|(name, proc)| format!("{}: {}\n", name, proc.description()))
            .collect::<Vec<String>>()
            .join("\n")
    }

    /// Advances the currently selected processor by one instruction.
    pub fn step(&mut self) {
        self.processors
            .get_mut(&self.selected_processor)
            .unwrap()
            .step()
    }
}

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
        processors.insert("cpu".to_string(), Box::new(Mos6502::new(ram)));
    } else {
        invalid_data!("unsupported binary file type");
    }
    return Ok(SimEnv {
        selected_processor: "cpu".to_string(),
        processors,
    });
}
