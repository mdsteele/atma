use std::rc::Rc;

//===========================================================================//

fn assemble(source: &str) -> Vec<u8> {
    let arch = "SuperFX";
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
    let alt = 0u8;
    let ramb = 0x70u8;
    let instruction =
        atma::dis::superfx::Instruction::decode(&*rom_bus, pc, alt);
    instruction.format(&*rom_bus, pc, ramb)
}

fn assert_asm(source: &str, binary: &[u8]) {
    let obj_data = assemble(source);
    assert_eq!(
        obj_data,
        binary,
        "Expected {source:?} to assemble to {binary:02x?}, but instead it \
         assembled to {obj_data:02x?}, which disassembles into {:?}",
        disassemble(&obj_data)
    );
}

fn assert_asm_dis(source: &str, binary: &[u8]) {
    let obj_data = assemble(source);
    let disassembled = disassemble(&obj_data);
    assert_eq!(
        obj_data, binary,
        "Expected {source:?} to assemble to {binary:02x?}, but instead it \
         assembled to {obj_data:02x?}, which disassembles into \
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
    assert_asm_dis("ADD R0", &[0x50]);
    assert_asm_dis("ADD R1", &[0x51]);
    assert_asm_dis("ADD R2", &[0x52]);
    assert_asm_dis("ADD R3", &[0x53]);
    assert_asm_dis("ADD R4", &[0x54]);
    assert_asm_dis("ADD R5", &[0x55]);
    assert_asm_dis("ADD R6", &[0x56]);
    assert_asm_dis("ADD R7", &[0x57]);
    assert_asm_dis("ADD R8", &[0x58]);
    assert_asm_dis("ADD R9", &[0x59]);
    assert_asm_dis("ADD R10", &[0x5a]);
    assert_asm_dis("ADD R11", &[0x5b]);
    assert_asm_dis("ADD R12", &[0x5c]);
    assert_asm_dis("ADD R13", &[0x5d]);
    assert_asm_dis("ADD R14", &[0x5e]);
    assert_asm_dis("ADD R15", &[0x5f]);
}

#[test]
fn assemble_and_reg_instructions() {
    assert_asm_dis("AND R1", &[0x71]);
    assert_asm_dis("AND R2", &[0x72]);
    assert_asm_dis("AND R3", &[0x73]);
    assert_asm_dis("AND R4", &[0x74]);
    assert_asm_dis("AND R5", &[0x75]);
    assert_asm_dis("AND R6", &[0x76]);
    assert_asm_dis("AND R7", &[0x77]);
    assert_asm_dis("AND R8", &[0x78]);
    assert_asm_dis("AND R9", &[0x79]);
    assert_asm_dis("AND R10", &[0x7a]);
    assert_asm_dis("AND R11", &[0x7b]);
    assert_asm_dis("AND R12", &[0x7c]);
    assert_asm_dis("AND R13", &[0x7d]);
    assert_asm_dis("AND R14", &[0x7e]);
    assert_asm_dis("AND R15", &[0x7f]);
}

#[test]
fn assemble_branch_instructions() {
    assert_asm_dis("BCC $0012", &[0x0c, 0x10]);
    assert_asm_dis("BCS $0010", &[0x0d, 0x0e]);
    assert_asm_dis("BEQ $ff82", &[0x09, 0x80]);
    assert_asm_dis("BGE $0012", &[0x06, 0x10]);
    assert_asm_dis("BLT $0012", &[0x07, 0x10]);
    assert_asm_dis("BMI $fff2", &[0x0b, 0xf0]);
    assert_asm_dis("BNE $ff83", &[0x08, 0x81]);
    assert_asm_dis("BPL $0081", &[0x0a, 0x7f]);
    assert_asm_dis("BRA $0012", &[0x05, 0x10]);
    assert_asm_dis("BVC $0000", &[0x0e, 0xfe]);
    assert_asm_dis("BVS $ffff", &[0x0f, 0xfd]);
}

#[test]
fn assemble_dec_instructions() {
    assert_asm_dis("DEC R0", &[0xe0]);
    assert_asm_dis("DEC R1", &[0xe1]);
    assert_asm_dis("DEC R2", &[0xe2]);
    assert_asm_dis("DEC R3", &[0xe3]);
    assert_asm_dis("DEC R4", &[0xe4]);
    assert_asm_dis("DEC R5", &[0xe5]);
    assert_asm_dis("DEC R6", &[0xe6]);
    assert_asm_dis("DEC R7", &[0xe7]);
    assert_asm_dis("DEC R8", &[0xe8]);
    assert_asm_dis("DEC R9", &[0xe9]);
    assert_asm_dis("DEC R10", &[0xea]);
    assert_asm_dis("DEC R11", &[0xeb]);
    assert_asm_dis("DEC R12", &[0xec]);
    assert_asm_dis("DEC R13", &[0xed]);
    assert_asm_dis("DEC R14", &[0xee]);
}

#[test]
fn assemble_from_instructions() {
    assert_asm_dis("FROM R0", &[0xb0]);
    assert_asm_dis("FROM R1", &[0xb1]);
    assert_asm_dis("FROM R2", &[0xb2]);
    assert_asm_dis("FROM R3", &[0xb3]);
    assert_asm_dis("FROM R4", &[0xb4]);
    assert_asm_dis("FROM R5", &[0xb5]);
    assert_asm_dis("FROM R6", &[0xb6]);
    assert_asm_dis("FROM R7", &[0xb7]);
    assert_asm_dis("FROM R8", &[0xb8]);
    assert_asm_dis("FROM R9", &[0xb9]);
    assert_asm_dis("FROM R10", &[0xba]);
    assert_asm_dis("FROM R11", &[0xbb]);
    assert_asm_dis("FROM R12", &[0xbc]);
    assert_asm_dis("FROM R13", &[0xbd]);
    assert_asm_dis("FROM R14", &[0xbe]);
    assert_asm_dis("FROM R15", &[0xbf]);
}

#[test]
fn assemble_ibt_instructions() {
    assert_asm_dis("IBT R0, #$12", &[0xa0, 0x12]);
    assert_asm_dis("IBT R1, #$34", &[0xa1, 0x34]);
    assert_asm_dis("IBT R2, #$56", &[0xa2, 0x56]);
    assert_asm_dis("IBT R3, #$78", &[0xa3, 0x78]);
    assert_asm_dis("IBT R4, #$9a", &[0xa4, 0x9a]);
    assert_asm_dis("IBT R5, #$bc", &[0xa5, 0xbc]);
    assert_asm_dis("IBT R6, #$de", &[0xa6, 0xde]);
    assert_asm_dis("IBT R7, #$f0", &[0xa7, 0xf0]);
    assert_asm_dis("IBT R8, #$00", &[0xa8, 0x00]);
    assert_asm_dis("IBT R9, #$80", &[0xa9, 0x80]);
    assert_asm_dis("IBT R10, #$ff", &[0xaa, 0xff]);
    assert_asm_dis("IBT R11, #$01", &[0xab, 0x01]);
    assert_asm_dis("IBT R12, #$7f", &[0xac, 0x7f]);
    assert_asm_dis("IBT R13, #$2d", &[0xad, 0x2d]);
    assert_asm_dis("IBT R14, #$99", &[0xae, 0x99]);
    assert_asm_dis("IBT R15, #$81", &[0xaf, 0x81]);
}

#[test]
fn assemble_inc_instructions() {
    assert_asm_dis("INC R0", &[0xd0]);
    assert_asm_dis("INC R1", &[0xd1]);
    assert_asm_dis("INC R2", &[0xd2]);
    assert_asm_dis("INC R3", &[0xd3]);
    assert_asm_dis("INC R4", &[0xd4]);
    assert_asm_dis("INC R5", &[0xd5]);
    assert_asm_dis("INC R6", &[0xd6]);
    assert_asm_dis("INC R7", &[0xd7]);
    assert_asm_dis("INC R8", &[0xd8]);
    assert_asm_dis("INC R9", &[0xd9]);
    assert_asm_dis("INC R10", &[0xda]);
    assert_asm_dis("INC R11", &[0xdb]);
    assert_asm_dis("INC R12", &[0xdc]);
    assert_asm_dis("INC R13", &[0xdd]);
    assert_asm_dis("INC R14", &[0xde]);
}

#[test]
fn assemble_iwt_instructions() {
    assert_asm_dis("IWT R0, #$1234", &[0xf0, 0x34, 0x12]);
    assert_asm_dis("IWT R1, #$3456", &[0xf1, 0x56, 0x34]);
    assert_asm_dis("IWT R2, #$5678", &[0xf2, 0x78, 0x56]);
    assert_asm_dis("IWT R3, #$789a", &[0xf3, 0x9a, 0x78]);
    assert_asm_dis("IWT R4, #$9abc", &[0xf4, 0xbc, 0x9a]);
    assert_asm_dis("IWT R5, #$bcde", &[0xf5, 0xde, 0xbc]);
    assert_asm_dis("IWT R6, #$def0", &[0xf6, 0xf0, 0xde]);
    assert_asm_dis("IWT R7, #$f012", &[0xf7, 0x12, 0xf0]);
    assert_asm_dis("IWT R8, #$0000", &[0xf8, 0x00, 0x00]);
    assert_asm_dis("IWT R9, #$8000", &[0xf9, 0x00, 0x80]);
    assert_asm_dis("IWT R10, #$ffff", &[0xfa, 0xff, 0xff]);
    assert_asm_dis("IWT R11, #$0001", &[0xfb, 0x01, 0x00]);
    assert_asm_dis("IWT R12, #$7fff", &[0xfc, 0xff, 0x7f]);
    assert_asm_dis("IWT R13, #$beef", &[0xfd, 0xef, 0xbe]);
    assert_asm_dis("IWT R14, #$9999", &[0xfe, 0x99, 0x99]);
    assert_asm_dis("IWT R15, #$8001", &[0xff, 0x01, 0x80]);
}

#[test]
fn assemble_jmp_instructions() {
    assert_asm_dis("JMP R8", &[0x98]);
    assert_asm_dis("JMP R9", &[0x99]);
    assert_asm_dis("JMP R10", &[0x9a]);
    assert_asm_dis("JMP R11", &[0x9b]);
    assert_asm_dis("JMP R12", &[0x9c]);
    assert_asm_dis("JMP R13", &[0x9d]);
}

#[test]
fn assemble_ldw_instructions() {
    assert_asm_dis("LDW (R0)", &[0x40]);
    assert_asm_dis("LDW (R1)", &[0x41]);
    assert_asm_dis("LDW (R2)", &[0x42]);
    assert_asm_dis("LDW (R3)", &[0x43]);
    assert_asm_dis("LDW (R4)", &[0x44]);
    assert_asm_dis("LDW (R5)", &[0x45]);
    assert_asm_dis("LDW (R6)", &[0x46]);
    assert_asm_dis("LDW (R7)", &[0x47]);
    assert_asm_dis("LDW (R8)", &[0x48]);
    assert_asm_dis("LDW (R9)", &[0x49]);
    assert_asm_dis("LDW (R10)", &[0x4a]);
    assert_asm_dis("LDW (R11)", &[0x4b]);
}

#[test]
fn assemble_link_instructions() {
    assert_asm_dis("LINK #1", &[0x91]);
    assert_asm_dis("LINK #2", &[0x92]);
    assert_asm_dis("LINK #3", &[0x93]);
    assert_asm_dis("LINK #4", &[0x94]);
    assert_asm("LINK $0002", &[0x91]);
    assert_asm("LINK $0003", &[0x92]);
    assert_asm("LINK $0004", &[0x93]);
    assert_asm("LINK $0005", &[0x94]);
}

#[test]
fn assemble_misc_instructions() {
    assert_asm_dis("ALT1", &[0x3d]);
    assert_asm_dis("ALT2", &[0x3e]);
    assert_asm_dis("ALT3", &[0x3f]);
    assert_asm_dis("ASR", &[0x96]);
    assert_asm_dis("COLOR", &[0x4e]);
    assert_asm_dis("FMULT", &[0x9f]);
    assert_asm_dis("GETB", &[0xef]);
    assert_asm_dis("GETC", &[0xdf]);
    assert_asm_dis("HIB", &[0xc0]);
    assert_asm_dis("LOB", &[0x9e]);
    assert_asm_dis("LOOP", &[0x3c]);
    assert_asm_dis("MERGE", &[0x70]);
    assert_asm_dis("NOP", &[0x01]);
    assert_asm_dis("NOT", &[0x4f]);
    assert_asm_dis("PLOT", &[0x4c]);
    assert_asm_dis("ROR", &[0x97]);
    assert_asm_dis("SBK", &[0x90]);
    assert_asm_dis("SEX", &[0x95]);
    assert_asm_dis("STOP", &[0x00]);
    assert_asm_dis("SWAP", &[0x4d]);
}

#[test]
fn assemble_mult_reg_instructions() {
    assert_asm_dis("MULT R0", &[0x80]);
    assert_asm_dis("MULT R1", &[0x81]);
    assert_asm_dis("MULT R2", &[0x82]);
    assert_asm_dis("MULT R3", &[0x83]);
    assert_asm_dis("MULT R4", &[0x84]);
    assert_asm_dis("MULT R5", &[0x85]);
    assert_asm_dis("MULT R6", &[0x86]);
    assert_asm_dis("MULT R7", &[0x87]);
    assert_asm_dis("MULT R8", &[0x88]);
    assert_asm_dis("MULT R9", &[0x89]);
    assert_asm_dis("MULT R10", &[0x8a]);
    assert_asm_dis("MULT R11", &[0x8b]);
    assert_asm_dis("MULT R12", &[0x8c]);
    assert_asm_dis("MULT R13", &[0x8d]);
    assert_asm_dis("MULT R14", &[0x8e]);
    assert_asm_dis("MULT R15", &[0x8f]);
}

#[test]
fn assemble_or_reg_instructions() {
    assert_asm_dis("OR R1", &[0xc1]);
    assert_asm_dis("OR R2", &[0xc2]);
    assert_asm_dis("OR R3", &[0xc3]);
    assert_asm_dis("OR R4", &[0xc4]);
    assert_asm_dis("OR R5", &[0xc5]);
    assert_asm_dis("OR R6", &[0xc6]);
    assert_asm_dis("OR R7", &[0xc7]);
    assert_asm_dis("OR R8", &[0xc8]);
    assert_asm_dis("OR R9", &[0xc9]);
    assert_asm_dis("OR R10", &[0xca]);
    assert_asm_dis("OR R11", &[0xcb]);
    assert_asm_dis("OR R12", &[0xcc]);
    assert_asm_dis("OR R13", &[0xcd]);
    assert_asm_dis("OR R14", &[0xce]);
    assert_asm_dis("OR R15", &[0xcf]);
}

#[test]
fn assemble_stw_instructions() {
    assert_asm_dis("STW (R0)", &[0x30]);
    assert_asm_dis("STW (R1)", &[0x31]);
    assert_asm_dis("STW (R2)", &[0x32]);
    assert_asm_dis("STW (R3)", &[0x33]);
    assert_asm_dis("STW (R4)", &[0x34]);
    assert_asm_dis("STW (R5)", &[0x35]);
    assert_asm_dis("STW (R6)", &[0x36]);
    assert_asm_dis("STW (R7)", &[0x37]);
    assert_asm_dis("STW (R8)", &[0x38]);
    assert_asm_dis("STW (R9)", &[0x39]);
    assert_asm_dis("STW (R10)", &[0x3a]);
    assert_asm_dis("STW (R11)", &[0x3b]);
}

#[test]
fn assemble_sub_reg_instructions() {
    assert_asm_dis("SUB R0", &[0x60]);
    assert_asm_dis("SUB R1", &[0x61]);
    assert_asm_dis("SUB R2", &[0x62]);
    assert_asm_dis("SUB R3", &[0x63]);
    assert_asm_dis("SUB R4", &[0x64]);
    assert_asm_dis("SUB R5", &[0x65]);
    assert_asm_dis("SUB R6", &[0x66]);
    assert_asm_dis("SUB R7", &[0x67]);
    assert_asm_dis("SUB R8", &[0x68]);
    assert_asm_dis("SUB R9", &[0x69]);
    assert_asm_dis("SUB R10", &[0x6a]);
    assert_asm_dis("SUB R11", &[0x6b]);
    assert_asm_dis("SUB R12", &[0x6c]);
    assert_asm_dis("SUB R13", &[0x6d]);
    assert_asm_dis("SUB R14", &[0x6e]);
    assert_asm_dis("SUB R15", &[0x6f]);
}

#[test]
fn assemble_to_instructions() {
    assert_asm_dis("TO R0", &[0x10]);
    assert_asm_dis("TO R1", &[0x11]);
    assert_asm_dis("TO R2", &[0x12]);
    assert_asm_dis("TO R3", &[0x13]);
    assert_asm_dis("TO R4", &[0x14]);
    assert_asm_dis("TO R5", &[0x15]);
    assert_asm_dis("TO R6", &[0x16]);
    assert_asm_dis("TO R7", &[0x17]);
    assert_asm_dis("TO R8", &[0x18]);
    assert_asm_dis("TO R9", &[0x19]);
    assert_asm_dis("TO R10", &[0x1a]);
    assert_asm_dis("TO R11", &[0x1b]);
    assert_asm_dis("TO R12", &[0x1c]);
    assert_asm_dis("TO R13", &[0x1d]);
    assert_asm_dis("TO R14", &[0x1e]);
    assert_asm_dis("TO R15", &[0x1f]);
}

#[test]
fn assemble_with_instructions() {
    assert_asm_dis("WITH R0", &[0x20]);
    assert_asm_dis("WITH R1", &[0x21]);
    assert_asm_dis("WITH R2", &[0x22]);
    assert_asm_dis("WITH R3", &[0x23]);
    assert_asm_dis("WITH R4", &[0x24]);
    assert_asm_dis("WITH R5", &[0x25]);
    assert_asm_dis("WITH R6", &[0x26]);
    assert_asm_dis("WITH R7", &[0x27]);
    assert_asm_dis("WITH R8", &[0x28]);
    assert_asm_dis("WITH R9", &[0x29]);
    assert_asm_dis("WITH R10", &[0x2a]);
    assert_asm_dis("WITH R11", &[0x2b]);
    assert_asm_dis("WITH R12", &[0x2c]);
    assert_asm_dis("WITH R13", &[0x2d]);
    assert_asm_dis("WITH R14", &[0x2e]);
    assert_asm_dis("WITH R15", &[0x2f]);
}

//===========================================================================//
