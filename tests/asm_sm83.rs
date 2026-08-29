use std::rc::Rc;

//===========================================================================//

fn assemble(source: &str) -> Vec<u8> {
    let arch = "SM83";
    let asm_path = Rc::<str>::from("input");
    let asm_source = Rc::<str>::from(format!(
        ".SECTION \"TEST\", arch=\"{arch}\", start=0\n{source}\n.END\n"
    ));
    let mut cache = atma::error::StrSrcCache::new();
    cache.add_source(asm_path.clone(), asm_source.clone());
    let obj_file =
        atma::asm::assemble_source(&mut cache, asm_path, &asm_source)
            .expect(&format!("Failed to assemble {source:?} for {arch}"));
    assert_eq!(obj_file.chunks.len(), 1);
    let obj_chunk = &obj_file.chunks[0];
    assert!(obj_chunk.patches.is_empty());
    obj_chunk.data.to_vec()
}

fn disassemble(binary: &[u8]) -> String {
    let mut rom_data = vec![0u8; binary.len().next_power_of_two()];
    rom_data[..binary.len()].copy_from_slice(binary);
    let rom_bus = atma::bus::new_rom_bus(rom_data.into_boxed_slice());
    let pc = 0;
    let instruction = atma::dis::sm83::Instruction::decode(&*rom_bus, pc);
    instruction.format(&*rom_bus, pc)
}

fn assert_asm_dis(source: &str, binary: &[u8]) {
    let obj_data = assemble(source);
    let disassembled = disassemble(&obj_data);
    assert_eq!(
        obj_data, binary,
        "Expected {source:?} to assemble to {binary:02x?}, but instead it \
         assembled to {obj_data:02x?}, which disassembled into \
         {disassembled:?}"
    );
    assert_eq!(
        source, disassembled,
        "{source:?} assembled into {obj_data:02x?}, but then disassembled \
         into {disassembled:?}"
    );
}

//===========================================================================//

#[test]
fn assemble_dec_instructions() {
    assert_asm_dis("DEC A", &[0x3d]);
    assert_asm_dis("DEC B", &[0x05]);
    assert_asm_dis("DEC C", &[0x0d]);
    assert_asm_dis("DEC D", &[0x15]);
    assert_asm_dis("DEC E", &[0x1d]);
    assert_asm_dis("DEC H", &[0x25]);
    assert_asm_dis("DEC L", &[0x2d]);
    assert_asm_dis("DEC [HL]", &[0x35]);
    assert_asm_dis("DEC BC", &[0x0b]);
    assert_asm_dis("DEC DE", &[0x1b]);
    assert_asm_dis("DEC HL", &[0x2b]);
    assert_asm_dis("DEC SP", &[0x3b]);
}

#[test]
fn assemble_inc_instructions() {
    assert_asm_dis("INC A", &[0x3c]);
    assert_asm_dis("INC B", &[0x04]);
    assert_asm_dis("INC C", &[0x0c]);
    assert_asm_dis("INC D", &[0x14]);
    assert_asm_dis("INC E", &[0x1c]);
    assert_asm_dis("INC H", &[0x24]);
    assert_asm_dis("INC L", &[0x2c]);
    assert_asm_dis("INC [HL]", &[0x34]);
    assert_asm_dis("INC BC", &[0x03]);
    assert_asm_dis("INC DE", &[0x13]);
    assert_asm_dis("INC HL", &[0x23]);
    assert_asm_dis("INC SP", &[0x33]);
}

#[test]
fn assemble_misc_instructions() {
    assert_asm_dis("CCF", &[0x3f]);
    assert_asm_dis("CPL", &[0x2f]);
    assert_asm_dis("DAA", &[0x27]);
    assert_asm_dis("DI", &[0xf3]);
    assert_asm_dis("EI", &[0xfb]);
    assert_asm_dis("HALT", &[0x76]);
    assert_asm_dis("NOP", &[0x00]);
    assert_asm_dis("RLA", &[0x17]);
    assert_asm_dis("RLCA", &[0x07]);
    assert_asm_dis("RRA", &[0x1f]);
    assert_asm_dis("RRCA", &[0x0f]);
    assert_asm_dis("SCF", &[0x37]);
    assert_asm_dis("STOP", &[0x10]);
}

#[test]
fn assemble_prefixed_rl_instructions() {
    assert_asm_dis("RL A", &[0xcb, 0x17]);
    assert_asm_dis("RL B", &[0xcb, 0x10]);
    assert_asm_dis("RL C", &[0xcb, 0x11]);
    assert_asm_dis("RL D", &[0xcb, 0x12]);
    assert_asm_dis("RL E", &[0xcb, 0x13]);
    assert_asm_dis("RL H", &[0xcb, 0x14]);
    assert_asm_dis("RL L", &[0xcb, 0x15]);
    assert_asm_dis("RL [HL]", &[0xcb, 0x16]);
}

#[test]
fn assemble_prefixed_rlc_instructions() {
    assert_asm_dis("RLC A", &[0xcb, 0x07]);
    assert_asm_dis("RLC B", &[0xcb, 0x00]);
    assert_asm_dis("RLC C", &[0xcb, 0x01]);
    assert_asm_dis("RLC D", &[0xcb, 0x02]);
    assert_asm_dis("RLC E", &[0xcb, 0x03]);
    assert_asm_dis("RLC H", &[0xcb, 0x04]);
    assert_asm_dis("RLC L", &[0xcb, 0x05]);
    assert_asm_dis("RLC [HL]", &[0xcb, 0x06]);
}

#[test]
fn assemble_prefixed_rr_instructions() {
    assert_asm_dis("RR A", &[0xcb, 0x1f]);
    assert_asm_dis("RR B", &[0xcb, 0x18]);
    assert_asm_dis("RR C", &[0xcb, 0x19]);
    assert_asm_dis("RR D", &[0xcb, 0x1a]);
    assert_asm_dis("RR E", &[0xcb, 0x1b]);
    assert_asm_dis("RR H", &[0xcb, 0x1c]);
    assert_asm_dis("RR L", &[0xcb, 0x1d]);
    assert_asm_dis("RR [HL]", &[0xcb, 0x1e]);
}

#[test]
fn assemble_prefixed_rrc_instructions() {
    assert_asm_dis("RRC A", &[0xcb, 0x0f]);
    assert_asm_dis("RRC B", &[0xcb, 0x08]);
    assert_asm_dis("RRC C", &[0xcb, 0x09]);
    assert_asm_dis("RRC D", &[0xcb, 0x0a]);
    assert_asm_dis("RRC E", &[0xcb, 0x0b]);
    assert_asm_dis("RRC H", &[0xcb, 0x0c]);
    assert_asm_dis("RRC L", &[0xcb, 0x0d]);
    assert_asm_dis("RRC [HL]", &[0xcb, 0x0e]);
}

#[test]
fn assemble_ret_instructions() {
    assert_asm_dis("RET", &[0xc9]);
    assert_asm_dis("RET C", &[0xd8]);
    assert_asm_dis("RET NC", &[0xd0]);
    assert_asm_dis("RET Z", &[0xc8]);
    assert_asm_dis("RET NZ", &[0xc0]);
    assert_asm_dis("RETI", &[0xd9]);
}

//===========================================================================//
