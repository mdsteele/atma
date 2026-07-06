use atma;
use std::rc::Rc;

//===========================================================================//

fn assert_asm(source: &str, binary: &[u8]) {
    let arch = "SuperFX";
    let asm_path = Rc::<str>::from("input");
    let asm_source = Rc::<str>::from(format!(
        ".SECTION \"TEST\", arch=\"{arch}\"\n{source}\n.END\n"
    ));
    let mut cache = atma::error::StrSrcCache::new();
    cache.add_source(asm_path.clone(), asm_source.clone());
    let obj_file =
        atma::asm::assemble_source(&mut cache, asm_path, &asm_source)
            .expect(&format!("Failed to assemble {source:?} for {arch}"));
    assert_eq!(obj_file.chunks.len(), 1);
    let obj_chunk = &obj_file.chunks[0];
    assert!(obj_chunk.patches.is_empty());
    let obj_data: &[u8] = &*obj_chunk.data;

    let mut rom_data = vec![0u8; obj_data.len().next_power_of_two()];
    rom_data[..obj_data.len()].copy_from_slice(obj_data);
    let rom_bus = atma::bus::new_rom_bus(rom_data.into_boxed_slice());
    let pc = 0;
    let alt = 0u8;
    let ramb = 0x70u8;
    let instruction =
        atma::dis::superfx::Instruction::decode(&*rom_bus, pc, alt);
    let disassembled = instruction.format(&*rom_bus, pc, ramb);

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
fn assemble_add_reg_instructions() {
    assert_asm("ADD R0", &[0x50]);
    assert_asm("ADD R1", &[0x51]);
    assert_asm("ADD R2", &[0x52]);
    assert_asm("ADD R3", &[0x53]);
    assert_asm("ADD R4", &[0x54]);
    assert_asm("ADD R5", &[0x55]);
    assert_asm("ADD R6", &[0x56]);
    assert_asm("ADD R7", &[0x57]);
    assert_asm("ADD R8", &[0x58]);
    assert_asm("ADD R9", &[0x59]);
    assert_asm("ADD R10", &[0x5a]);
    assert_asm("ADD R11", &[0x5b]);
    assert_asm("ADD R12", &[0x5c]);
    assert_asm("ADD R13", &[0x5d]);
    assert_asm("ADD R14", &[0x5e]);
    assert_asm("ADD R15", &[0x5f]);
}

#[test]
fn assemble_and_reg_instructions() {
    assert_asm("AND R1", &[0x71]);
    assert_asm("AND R2", &[0x72]);
    assert_asm("AND R3", &[0x73]);
    assert_asm("AND R4", &[0x74]);
    assert_asm("AND R5", &[0x75]);
    assert_asm("AND R6", &[0x76]);
    assert_asm("AND R7", &[0x77]);
    assert_asm("AND R8", &[0x78]);
    assert_asm("AND R9", &[0x79]);
    assert_asm("AND R10", &[0x7a]);
    assert_asm("AND R11", &[0x7b]);
    assert_asm("AND R12", &[0x7c]);
    assert_asm("AND R13", &[0x7d]);
    assert_asm("AND R14", &[0x7e]);
    assert_asm("AND R15", &[0x7f]);
}

#[test]
fn assemble_dec_instructions() {
    assert_asm("DEC R0", &[0xe0]);
    assert_asm("DEC R1", &[0xe1]);
    assert_asm("DEC R2", &[0xe2]);
    assert_asm("DEC R3", &[0xe3]);
    assert_asm("DEC R4", &[0xe4]);
    assert_asm("DEC R5", &[0xe5]);
    assert_asm("DEC R6", &[0xe6]);
    assert_asm("DEC R7", &[0xe7]);
    assert_asm("DEC R8", &[0xe8]);
    assert_asm("DEC R9", &[0xe9]);
    assert_asm("DEC R10", &[0xea]);
    assert_asm("DEC R11", &[0xeb]);
    assert_asm("DEC R12", &[0xec]);
    assert_asm("DEC R13", &[0xed]);
    assert_asm("DEC R14", &[0xee]);
}

#[test]
fn assemble_from_instructions() {
    assert_asm("FROM R0", &[0xb0]);
    assert_asm("FROM R1", &[0xb1]);
    assert_asm("FROM R2", &[0xb2]);
    assert_asm("FROM R3", &[0xb3]);
    assert_asm("FROM R4", &[0xb4]);
    assert_asm("FROM R5", &[0xb5]);
    assert_asm("FROM R6", &[0xb6]);
    assert_asm("FROM R7", &[0xb7]);
    assert_asm("FROM R8", &[0xb8]);
    assert_asm("FROM R9", &[0xb9]);
    assert_asm("FROM R10", &[0xba]);
    assert_asm("FROM R11", &[0xbb]);
    assert_asm("FROM R12", &[0xbc]);
    assert_asm("FROM R13", &[0xbd]);
    assert_asm("FROM R14", &[0xbe]);
    assert_asm("FROM R15", &[0xbf]);
}

#[test]
fn assemble_ibt_instructions() {
    assert_asm("IBT R0, #$12", &[0xa0, 0x12]);
    assert_asm("IBT R1, #$34", &[0xa1, 0x34]);
    assert_asm("IBT R2, #$56", &[0xa2, 0x56]);
    assert_asm("IBT R3, #$78", &[0xa3, 0x78]);
    assert_asm("IBT R4, #$9a", &[0xa4, 0x9a]);
    assert_asm("IBT R5, #$bc", &[0xa5, 0xbc]);
    assert_asm("IBT R6, #$de", &[0xa6, 0xde]);
    assert_asm("IBT R7, #$f0", &[0xa7, 0xf0]);
    assert_asm("IBT R8, #$00", &[0xa8, 0x00]);
    assert_asm("IBT R9, #$80", &[0xa9, 0x80]);
    assert_asm("IBT R10, #$ff", &[0xaa, 0xff]);
    assert_asm("IBT R11, #$01", &[0xab, 0x01]);
    assert_asm("IBT R12, #$7f", &[0xac, 0x7f]);
    assert_asm("IBT R13, #$2d", &[0xad, 0x2d]);
    assert_asm("IBT R14, #$99", &[0xae, 0x99]);
    assert_asm("IBT R15, #$81", &[0xaf, 0x81]);
}

#[test]
fn assemble_inc_instructions() {
    assert_asm("INC R0", &[0xd0]);
    assert_asm("INC R1", &[0xd1]);
    assert_asm("INC R2", &[0xd2]);
    assert_asm("INC R3", &[0xd3]);
    assert_asm("INC R4", &[0xd4]);
    assert_asm("INC R5", &[0xd5]);
    assert_asm("INC R6", &[0xd6]);
    assert_asm("INC R7", &[0xd7]);
    assert_asm("INC R8", &[0xd8]);
    assert_asm("INC R9", &[0xd9]);
    assert_asm("INC R10", &[0xda]);
    assert_asm("INC R11", &[0xdb]);
    assert_asm("INC R12", &[0xdc]);
    assert_asm("INC R13", &[0xdd]);
    assert_asm("INC R14", &[0xde]);
}

#[test]
fn assemble_iwt_instructions() {
    assert_asm("IWT R0, #$1234", &[0xf0, 0x34, 0x12]);
    assert_asm("IWT R1, #$3456", &[0xf1, 0x56, 0x34]);
    assert_asm("IWT R2, #$5678", &[0xf2, 0x78, 0x56]);
    assert_asm("IWT R3, #$789a", &[0xf3, 0x9a, 0x78]);
    assert_asm("IWT R4, #$9abc", &[0xf4, 0xbc, 0x9a]);
    assert_asm("IWT R5, #$bcde", &[0xf5, 0xde, 0xbc]);
    assert_asm("IWT R6, #$def0", &[0xf6, 0xf0, 0xde]);
    assert_asm("IWT R7, #$f012", &[0xf7, 0x12, 0xf0]);
    assert_asm("IWT R8, #$0000", &[0xf8, 0x00, 0x00]);
    assert_asm("IWT R9, #$8000", &[0xf9, 0x00, 0x80]);
    assert_asm("IWT R10, #$ffff", &[0xfa, 0xff, 0xff]);
    assert_asm("IWT R11, #$0001", &[0xfb, 0x01, 0x00]);
    assert_asm("IWT R12, #$7fff", &[0xfc, 0xff, 0x7f]);
    assert_asm("IWT R13, #$beef", &[0xfd, 0xef, 0xbe]);
    assert_asm("IWT R14, #$9999", &[0xfe, 0x99, 0x99]);
    assert_asm("IWT R15, #$8001", &[0xff, 0x01, 0x80]);
}

#[test]
fn assemble_jmp_instructions() {
    assert_asm("JMP R8", &[0x98]);
    assert_asm("JMP R9", &[0x99]);
    assert_asm("JMP R10", &[0x9a]);
    assert_asm("JMP R11", &[0x9b]);
    assert_asm("JMP R12", &[0x9c]);
    assert_asm("JMP R13", &[0x9d]);
}

#[test]
fn assemble_ldw_instructions() {
    assert_asm("LDW (R0)", &[0x40]);
    assert_asm("LDW (R1)", &[0x41]);
    assert_asm("LDW (R2)", &[0x42]);
    assert_asm("LDW (R3)", &[0x43]);
    assert_asm("LDW (R4)", &[0x44]);
    assert_asm("LDW (R5)", &[0x45]);
    assert_asm("LDW (R6)", &[0x46]);
    assert_asm("LDW (R7)", &[0x47]);
    assert_asm("LDW (R8)", &[0x48]);
    assert_asm("LDW (R9)", &[0x49]);
    assert_asm("LDW (R10)", &[0x4a]);
    assert_asm("LDW (R11)", &[0x4b]);
}

#[test]
fn assemble_link_instructions() {
    assert_asm("LINK #1", &[0x91]);
    assert_asm("LINK #2", &[0x92]);
    assert_asm("LINK #3", &[0x93]);
    assert_asm("LINK #4", &[0x94]);
}

#[test]
fn assemble_misc_instructions() {
    assert_asm("ALT1", &[0x3d]);
    assert_asm("ALT2", &[0x3e]);
    assert_asm("ALT3", &[0x3f]);
    assert_asm("ASR", &[0x96]);
    assert_asm("COLOR", &[0x4e]);
    assert_asm("FMULT", &[0x9f]);
    assert_asm("GETB", &[0xef]);
    assert_asm("GETC", &[0xdf]);
    assert_asm("HIB", &[0xc0]);
    assert_asm("LOB", &[0x9e]);
    assert_asm("LOOP", &[0x3c]);
    assert_asm("MERGE", &[0x70]);
    assert_asm("NOP", &[0x01]);
    assert_asm("NOT", &[0x4f]);
    assert_asm("PLOT", &[0x4c]);
    assert_asm("ROR", &[0x97]);
    assert_asm("SBK", &[0x90]);
    assert_asm("SEX", &[0x95]);
    assert_asm("STOP", &[0x00]);
    assert_asm("SWAP", &[0x4d]);
}

#[test]
fn assemble_mult_reg_instructions() {
    assert_asm("MULT R0", &[0x80]);
    assert_asm("MULT R1", &[0x81]);
    assert_asm("MULT R2", &[0x82]);
    assert_asm("MULT R3", &[0x83]);
    assert_asm("MULT R4", &[0x84]);
    assert_asm("MULT R5", &[0x85]);
    assert_asm("MULT R6", &[0x86]);
    assert_asm("MULT R7", &[0x87]);
    assert_asm("MULT R8", &[0x88]);
    assert_asm("MULT R9", &[0x89]);
    assert_asm("MULT R10", &[0x8a]);
    assert_asm("MULT R11", &[0x8b]);
    assert_asm("MULT R12", &[0x8c]);
    assert_asm("MULT R13", &[0x8d]);
    assert_asm("MULT R14", &[0x8e]);
    assert_asm("MULT R15", &[0x8f]);
}

#[test]
fn assemble_or_reg_instructions() {
    assert_asm("OR R1", &[0xc1]);
    assert_asm("OR R2", &[0xc2]);
    assert_asm("OR R3", &[0xc3]);
    assert_asm("OR R4", &[0xc4]);
    assert_asm("OR R5", &[0xc5]);
    assert_asm("OR R6", &[0xc6]);
    assert_asm("OR R7", &[0xc7]);
    assert_asm("OR R8", &[0xc8]);
    assert_asm("OR R9", &[0xc9]);
    assert_asm("OR R10", &[0xca]);
    assert_asm("OR R11", &[0xcb]);
    assert_asm("OR R12", &[0xcc]);
    assert_asm("OR R13", &[0xcd]);
    assert_asm("OR R14", &[0xce]);
    assert_asm("OR R15", &[0xcf]);
}

#[test]
fn assemble_stw_instructions() {
    assert_asm("STW (R0)", &[0x30]);
    assert_asm("STW (R1)", &[0x31]);
    assert_asm("STW (R2)", &[0x32]);
    assert_asm("STW (R3)", &[0x33]);
    assert_asm("STW (R4)", &[0x34]);
    assert_asm("STW (R5)", &[0x35]);
    assert_asm("STW (R6)", &[0x36]);
    assert_asm("STW (R7)", &[0x37]);
    assert_asm("STW (R8)", &[0x38]);
    assert_asm("STW (R9)", &[0x39]);
    assert_asm("STW (R10)", &[0x3a]);
    assert_asm("STW (R11)", &[0x3b]);
}

#[test]
fn assemble_sub_reg_instructions() {
    assert_asm("SUB R0", &[0x60]);
    assert_asm("SUB R1", &[0x61]);
    assert_asm("SUB R2", &[0x62]);
    assert_asm("SUB R3", &[0x63]);
    assert_asm("SUB R4", &[0x64]);
    assert_asm("SUB R5", &[0x65]);
    assert_asm("SUB R6", &[0x66]);
    assert_asm("SUB R7", &[0x67]);
    assert_asm("SUB R8", &[0x68]);
    assert_asm("SUB R9", &[0x69]);
    assert_asm("SUB R10", &[0x6a]);
    assert_asm("SUB R11", &[0x6b]);
    assert_asm("SUB R12", &[0x6c]);
    assert_asm("SUB R13", &[0x6d]);
    assert_asm("SUB R14", &[0x6e]);
    assert_asm("SUB R15", &[0x6f]);
}

#[test]
fn assemble_to_instructions() {
    assert_asm("TO R0", &[0x10]);
    assert_asm("TO R1", &[0x11]);
    assert_asm("TO R2", &[0x12]);
    assert_asm("TO R3", &[0x13]);
    assert_asm("TO R4", &[0x14]);
    assert_asm("TO R5", &[0x15]);
    assert_asm("TO R6", &[0x16]);
    assert_asm("TO R7", &[0x17]);
    assert_asm("TO R8", &[0x18]);
    assert_asm("TO R9", &[0x19]);
    assert_asm("TO R10", &[0x1a]);
    assert_asm("TO R11", &[0x1b]);
    assert_asm("TO R12", &[0x1c]);
    assert_asm("TO R13", &[0x1d]);
    assert_asm("TO R14", &[0x1e]);
    assert_asm("TO R15", &[0x1f]);
}

#[test]
fn assemble_with_instructions() {
    assert_asm("WITH R0", &[0x20]);
    assert_asm("WITH R1", &[0x21]);
    assert_asm("WITH R2", &[0x22]);
    assert_asm("WITH R3", &[0x23]);
    assert_asm("WITH R4", &[0x24]);
    assert_asm("WITH R5", &[0x25]);
    assert_asm("WITH R6", &[0x26]);
    assert_asm("WITH R7", &[0x27]);
    assert_asm("WITH R8", &[0x28]);
    assert_asm("WITH R9", &[0x29]);
    assert_asm("WITH R10", &[0x2a]);
    assert_asm("WITH R11", &[0x2b]);
    assert_asm("WITH R12", &[0x2c]);
    assert_asm("WITH R13", &[0x2d]);
    assert_asm("WITH R14", &[0x2e]);
    assert_asm("WITH R15", &[0x2f]);
}

//===========================================================================//
