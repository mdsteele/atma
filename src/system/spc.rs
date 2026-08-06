use super::sim::SimSystem;
use crate::addr::Addr;
use crate::bus::{new_ram_bus, new_ssmp_bus};
use crate::proc::{SimProc, Spc700};
use std::io::{self, Read, Seek, SeekFrom};
use std::rc::Rc;

//===========================================================================//

/// Reads a compiled SPC binary file, and returns a simulated system that
/// represents it.
pub fn load_spc_binary<R: Read + Seek>(
    mut reader: R,
) -> io::Result<SimSystem> {
    // See https://wiki.superfamicom.org/spc-and-rsn-file-format
    let mut header = [0u8; 0x2e];
    reader.read_exact(&mut header)?;
    if &header[0..0x21] != b"SNES-SPC700 Sound File Data v0.30" {
        let message = "incorrect format name in SPC header";
        return Err(io::Error::other(message));
    }
    let minor_version = header[0x24];
    if minor_version != 30 {
        let message =
            format!("unsupported SPC minor version: {minor_version}");
        return Err(io::Error::other(message));
    }
    let id666_indicator = header[0x23];
    if id666_indicator != 26 {
        let message =
            format!("unsupported ID666 indicator byte: {id666_indicator}");
        return Err(io::Error::other(message));
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

    let bus = new_ssmp_bus(new_ram_bus(ram.into_boxed_slice()));
    let mut cpu: Box<dyn SimProc> = Box::new(Spc700::new());
    cpu.set_pc(Addr::from(pc));
    cpu.set_register("A", u32::from(reg_a));
    cpu.set_register("X", u32::from(reg_x));
    cpu.set_register("Y", u32::from(reg_y));
    cpu.set_register("PSW", u32::from(reg_psw));
    cpu.set_register("SP", u32::from(reg_sp));
    let processors = vec![(Rc::from("cpu"), (cpu, bus))];
    Ok(SimSystem::new(processors))
}

//===========================================================================//
