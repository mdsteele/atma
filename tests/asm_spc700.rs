use std::rc::Rc;

//===========================================================================//

fn assemble(source: &str) -> Vec<u8> {
    let arch = "SPC700";
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
    let instruction = atma::dis::spc700::Instruction::decode(&*rom_bus, pc);
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
fn assemble_branch_instructions() {
    assert_asm_dis("BCC $0042", &[0x90, 0x40]);
    assert_asm_dis("BCS $0081", &[0xb0, 0x7f]);
    assert_asm_dis("BNE $ff82", &[0xd0, 0x80]);
    assert_asm_dis("BEQ $0002", &[0xf0, 0x00]);
    assert_asm_dis("BPL $0000", &[0x10, 0xfe]);
    assert_asm_dis("BMI $ffff", &[0x30, 0xfd]);
    assert_asm_dis("BRA $0003", &[0x2f, 0x01]);
    assert_asm_dis("BVC $0004", &[0x50, 0x02]);
    assert_asm_dis("BVS $ffc2", &[0x70, 0xc0]);
}

#[test]
fn assemble_call_instructions() {
    assert_asm_dis("CALL !$1234", &[0x3f, 0x34, 0x12]);
    assert_asm_dis("PCALL $ff37", &[0x4f, 0x37]);
}

#[test]
fn assemble_flag_instructions() {
    assert_asm_dis("CLRC", &[0x60]);
    assert_asm_dis("CLRP", &[0x20]);
    assert_asm_dis("CLRV", &[0xe0]);
    assert_asm_dis("DI", &[0xc0]);
    assert_asm_dis("EI", &[0xa0]);
    assert_asm_dis("NOTC", &[0xed]);
    assert_asm_dis("SETC", &[0x80]);
    assert_asm_dis("SETP", &[0x40]);
}

#[test]
fn assemble_inc_dec_instructions() {
    assert_asm_dis("DEC A", &[0x9c]);
    assert_asm_dis("DEC X", &[0x1d]);
    assert_asm_dis("DEC Y", &[0xdc]);
    assert_asm_dis("DEC $12", &[0x8b, 0x12]);
    assert_asm_dis("DEC $34 + X", &[0x9b, 0x34]);
    assert_asm_dis("DEC !$1234", &[0x8c, 0x34, 0x12]);
    assert_asm_dis("DECW $56", &[0x1a, 0x56]);
    assert_asm_dis("INC A", &[0xbc]);
    assert_asm_dis("INC X", &[0x3d]);
    assert_asm_dis("INC Y", &[0xfc]);
    assert_asm_dis("INC $12", &[0xab, 0x12]);
    assert_asm_dis("INC $34 + X", &[0xbb, 0x34]);
    assert_asm_dis("INC !$1234", &[0xac, 0x34, 0x12]);
    assert_asm_dis("INCW $56", &[0x3a, 0x56]);
}

#[test]
fn assemble_jump_instructions() {
    assert_asm_dis("JMP !$1234", &[0x5f, 0x34, 0x12]);
    assert_asm_dis("JMP [!$1234 + X]", &[0x1f, 0x34, 0x12]);
}

#[test]
fn assemble_misc_instructions() {
    assert_asm_dis("BRK", &[0x0f]);
    assert_asm_dis("DAA A", &[0xdf]);
    assert_asm_dis("DAS A", &[0xbe]);
    assert_asm_dis("DIV YA, X", &[0x9e]);
    assert_asm_dis("MUL YA", &[0xcf]);
    assert_asm_dis("NOP", &[0x00]);
    assert_asm_dis("RET", &[0x6f]);
    assert_asm_dis("RETI", &[0x7f]);
    assert_asm_dis("SLEEP", &[0xef]);
    assert_asm_dis("STOP", &[0xff]);
    assert_asm_dis("XCN A", &[0x9f]);
}

#[test]
fn assemble_move_instructions() {
    assert_asm_dis("MOV A, #$00", &[0xe8, 0x00]);
    assert_asm_dis("MOV A, (X)", &[0xe6]);
    assert_asm_dis("MOV A, (X)+", &[0xbf]);
    assert_asm_dis("MOV A, $f5", &[0xe4, 0xf5]);
    assert_asm_dis("MOV A, $f5 + X", &[0xf4, 0xf5]);
    assert_asm_dis("MOV A, !$1234", &[0xe5, 0x34, 0x12]);
    assert_asm_dis("MOV A, !$1234 + X", &[0xf5, 0x34, 0x12]);
    assert_asm_dis("MOV A, !$1234 + Y", &[0xf6, 0x34, 0x12]);
    assert_asm_dis("MOV A, [$cd + X]", &[0xe7, 0xcd]);
    assert_asm_dis("MOV A, [$cd] + Y", &[0xf7, 0xcd]);
    assert_asm_dis("MOV X, #$ef", &[0xcd, 0xef]);
    assert_asm_dis("MOV X, $f4", &[0xf8, 0xf4]);
    assert_asm_dis("MOV X, $f4 + Y", &[0xf9, 0xf4]);
    assert_asm_dis("MOV X, !$1234", &[0xe9, 0x34, 0x12]);
    assert_asm_dis("MOV Y, #$ef", &[0x8d, 0xef]);
    assert_asm_dis("MOV Y, $f4", &[0xeb, 0xf4]);
    assert_asm_dis("MOV Y, $f4 + X", &[0xfb, 0xf4]);
    assert_asm_dis("MOV Y, !$1234", &[0xec, 0x34, 0x12]);
    assert_asm_dis("MOV (X), A", &[0xc6]);
    assert_asm_dis("MOV (X)+, A", &[0xaf]);
    assert_asm_dis("MOV $fe, A", &[0xc4, 0xfe]);
    assert_asm_dis("MOV $fe + X, A", &[0xd4, 0xfe]);
    assert_asm_dis("MOV !$1234, A", &[0xc5, 0x34, 0x12]);
    assert_asm_dis("MOV !$1234 + X, A", &[0xd5, 0x34, 0x12]);
    assert_asm_dis("MOV !$1234 + Y, A", &[0xd6, 0x34, 0x12]);
    assert_asm_dis("MOV [$fe + X], A", &[0xc7, 0xfe]);
    assert_asm_dis("MOV [$fe] + Y, A", &[0xd7, 0xfe]);
    assert_asm_dis("MOV $f4, X", &[0xd8, 0xf4]);
    assert_asm_dis("MOV $f4 + Y, X", &[0xd9, 0xf4]);
    assert_asm_dis("MOV !$1234, X", &[0xc9, 0x34, 0x12]);
    assert_asm_dis("MOV $f4, Y", &[0xcb, 0xf4]);
    assert_asm_dis("MOV $f4 + X, Y", &[0xdb, 0xf4]);
    assert_asm_dis("MOV !$1234, Y", &[0xcc, 0x34, 0x12]);
    assert_asm_dis("MOV A, X", &[0x7d]);
    assert_asm_dis("MOV A, Y", &[0xdd]);
    assert_asm_dis("MOV X, A", &[0x5d]);
    assert_asm_dis("MOV Y, A", &[0xfd]);
    assert_asm_dis("MOV X, SP", &[0x9d]);
    assert_asm_dis("MOV SP, X", &[0xbd]);
    assert_asm_dis("MOV $f4, $bb", &[0xfa, 0xbb, 0xf4]);
    assert_asm_dis("MOV $f4, #$aa", &[0x8f, 0xaa, 0xf4]);
}

#[test]
fn assemble_push_pop_instructions() {
    assert_asm_dis("PUSH A", &[0x2d]);
    assert_asm_dis("PUSH X", &[0x4d]);
    assert_asm_dis("PUSH Y", &[0x6d]);
    assert_asm_dis("PUSH PSW", &[0x0d]);
    assert_asm_dis("POP A", &[0xae]);
    assert_asm_dis("POP X", &[0xce]);
    assert_asm_dis("POP Y", &[0xee]);
    assert_asm_dis("POP PSW", &[0x8e]);
}

#[test]
fn assemble_rotate_instructions() {
    assert_asm_dis("ROL A", &[0x3c]);
    assert_asm_dis("ROL $12", &[0x2b, 0x12]);
    assert_asm_dis("ROL $34 + X", &[0x3b, 0x34]);
    assert_asm_dis("ROL !$1234", &[0x2c, 0x34, 0x12]);
    assert_asm_dis("ROR A", &[0x7c]);
    assert_asm_dis("ROR $12", &[0x6b, 0x12]);
    assert_asm_dis("ROR $34 + X", &[0x7b, 0x34]);
    assert_asm_dis("ROR !$1234", &[0x6c, 0x34, 0x12]);
}

#[test]
fn assemble_shift_instructions() {
    assert_asm_dis("ASL A", &[0x1c]);
    assert_asm_dis("ASL $12", &[0x0b, 0x12]);
    assert_asm_dis("ASL $34 + X", &[0x1b, 0x34]);
    assert_asm_dis("ASL !$1234", &[0x0c, 0x34, 0x12]);
    assert_asm_dis("LSR A", &[0x5c]);
    assert_asm_dis("LSR $12", &[0x4b, 0x12]);
    assert_asm_dis("LSR $34 + X", &[0x5b, 0x34]);
    assert_asm_dis("LSR !$1234", &[0x4c, 0x34, 0x12]);
}

//===========================================================================//
