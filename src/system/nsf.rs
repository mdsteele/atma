use super::sim::SimSystem;
use crate::addr::Addr;
use crate::bus::{NesBus, SimBus, new_nsf_bus};
use crate::proc::{Mos6502, SimProc};
use std::io::{self, Read, Seek};
use std::rc::Rc;

//===========================================================================//

/// Reads a compiled NSF binary file, and returns a simulated system that
/// represents it.
pub fn load_nsf_binary<R: Read + Seek>(
    mut reader: R,
) -> io::Result<SimSystem> {
    // See https://www.nesdev.org/wiki/NSF
    let mut header = [0u8; 0x80];
    reader.read_exact(&mut header)?;

    let version = header[0x05];
    if version != 1 {
        let message = format!("NSF version {version} is not supported");
        return Err(io::Error::other(message));
    }

    let total_songs = header[0x06];
    if total_songs == 0 {
        let message = "cannot load NSF with 0 total songs";
        return Err(io::Error::other(message));
    }

    let starting_song = header[0x07];
    if !(1..=total_songs).contains(&starting_song) {
        let message = format!(
            "invalid NSF starting song {starting_song} \
             (out of {total_songs} total songs)"
        );
        return Err(io::Error::other(message));
    }

    let load_addr = (u16::from(header[0x09]) << 8) | u16::from(header[0x08]);
    if load_addr < 0x8000 {
        let message = format!("invalid NSF load address: ${load_addr:04x}");
        return Err(io::Error::other(message));
    }
    let init_addr = (u16::from(header[0x0b]) << 8) | u16::from(header[0x0a]);
    if init_addr < 0x8000 {
        let message = format!("invalid NSF init address: ${init_addr:04x}");
        return Err(io::Error::other(message));
    }
    let play_addr = (u16::from(header[0x0d]) << 8) | u16::from(header[0x0c]);
    if play_addr < 0x8000 {
        let message = format!("invalid NSF play address: ${play_addr:04x}");
        return Err(io::Error::other(message));
    }

    if header[0x70..0x78] != [0, 0, 0, 0, 0, 0, 0, 0] {
        let message = "NSF bank switching is not yet supported";
        return Err(io::Error::other(message));
    }

    let platform = header[0x7a];
    if (platform & 0x03) != 0 {
        let message = format!("invalid NSF NTSC/PAL byte: ${platform:02x}");
        return Err(io::Error::other(message));
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
            let message = format!(
                "cannot load ${data_len:x}-byte data payload at address \
                 ${load_addr:04x}"
            );
            return Err(io::Error::other(message));
        }
        rom_data[start..end].copy_from_slice(&load_data);
        rom_data.into_boxed_slice()
    };

    let cart = new_nsf_bus(rom_data, init_addr, play_addr);
    let bus: Box<dyn SimBus> = Box::new(NesBus::with_cartridge(cart));
    let mut cpu: Box<dyn SimProc> = Box::new(Mos6502::new());
    cpu.set_pc(Addr::from(0x4800u16));
    cpu.set_register("A", u32::from(starting_song - 1));
    cpu.set_register("X", u32::from(platform & 0x01));
    cpu.set_register("S", 0xff);
    let processors = vec![(Rc::from("cpu"), (cpu, bus))];
    Ok(SimSystem::new(processors))
}

//===========================================================================//
