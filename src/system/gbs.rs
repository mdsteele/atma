use super::sim::SimSystem;
use crate::bus::{DmgBus, Mbc5Bus, SimBus, new_ram_bus, new_rom_bus};
use crate::proc::{SharpSm83, SimProc};
use std::io::{self, Read, Seek};
use std::rc::Rc;

//===========================================================================//

/// Reads a compiled GBS binary file, and returns a simulated system that
/// represents it.
pub fn load_gbs_binary<R: Read + Seek>(
    mut reader: R,
) -> io::Result<SimSystem> {
    // See https://ocremix.org/info/GBS_Format_Specification
    let mut header = [0u8; 0x70];
    reader.read_exact(&mut header)?;

    if &header[0..3] != b"GBS" {
        let message = "incorrect magic number for a GBS file";
        return Err(io::Error::other(message));
    }

    let version = header[0x03];
    if version != 1 {
        let message = format!("GBS version {version} is not supported");
        return Err(io::Error::other(message));
    }

    let total_songs = header[0x04];
    if total_songs == 0 {
        let message = "cannot load GBS with 0 total songs";
        return Err(io::Error::other(message));
    }

    let starting_song = header[0x05];
    if !(1..=total_songs).contains(&starting_song) {
        let message = format!(
            "invalid GBS starting song {starting_song} \
             (out of {total_songs} total songs)"
        );
        return Err(io::Error::other(message));
    }

    let load_addr = (u16::from(header[0x07]) << 8) | u16::from(header[0x06]);
    if !(0x400..0x8000).contains(&load_addr) {
        let message = format!("invalid GBS load address: ${load_addr:04x}");
        return Err(io::Error::other(message));
    }
    let init_addr = (u16::from(header[0x09]) << 8) | u16::from(header[0x08]);
    if !(0x400..0x8000).contains(&init_addr) {
        let message = format!("invalid GBS init address: ${init_addr:04x}");
        return Err(io::Error::other(message));
    }
    let play_addr = (u16::from(header[0x0b]) << 8) | u16::from(header[0x0a]);
    if !(0x400..0x8000).contains(&play_addr) {
        let message = format!("invalid GBS play address: ${play_addr:04x}");
        return Err(io::Error::other(message));
    }
    let stack_pointer =
        (u16::from(header[0x0d]) << 8) | u16::from(header[0x0c]);

    let mut load_data = Vec::<u8>::new();
    reader.read_to_end(&mut load_data)?;
    let rom_size =
        (usize::from(load_addr) + load_data.len()).next_power_of_two();
    let num_banks = rom_size.div_ceil(0x4000);
    if num_banks > 256 {
        let message = format!("too much GBS ROM data ({num_banks} 16k banks)");
        return Err(io::Error::other(message));
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
    let processors = vec![(Rc::from("cpu"), (cpu, bus))];
    Ok(SimSystem::new(processors))
}

//===========================================================================//
