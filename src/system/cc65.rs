use super::sim::SimSystem;
use crate::bus::{SimBus, new_ram_bus};
use crate::proc::{Mos6502, SimProc};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::rc::Rc;

//===========================================================================//

/// Reads a compiled NSF binary file, and returns a simulated system that
/// represents it.
pub fn load_sim65_binary<R: Read + Seek>(
    mut reader: R,
) -> io::Result<SimSystem> {
    let mut header = [0u8; 8];
    reader.read_exact(&mut header)?;
    if &header[..5] != b"sim65" {
        let message = "incorrect magic number in sim65 header";
        return Err(io::Error::other(message));
    }
    let mut processors =
        Vec::<(Rc<str>, (Box<dyn SimProc>, Box<dyn SimBus>))>::new();
    // Read rest of sim65 header.
    let version = header[5];
    if version != 2 {
        let message = format!("unsupported sim65 binary version: {}", version);
        return Err(io::Error::other(message));
    }
    let cpu_type = header[6];
    if cpu_type != 0 {
        let message = format!("unsupported sim65 cpu type: {}", cpu_type);
        return Err(io::Error::other(message));
    }
    let load_addr = reader.read_u16::<LittleEndian>()?;
    let reset_addr = reader.read_u16::<LittleEndian>()?;
    // Read sim65 code payload.
    let mut code = Vec::<u8>::new();
    reader.read_to_end(&mut code)?;
    if load_addr as usize + code.len() > 0x10000 {
        let message = "too much data";
        return Err(io::Error::other(message));
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
    processors.push((Rc::from("cpu"), (cpu, bus)));
    Ok(SimSystem::new(processors))
}

//===========================================================================//
