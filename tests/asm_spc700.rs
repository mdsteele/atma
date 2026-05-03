use atma;

//===========================================================================//

fn assert_asm(source: &str, binary: &[u8]) {
    let arch = "SPC700";
    let obj_source =
        format!(".SECTION \"TEST\", arch=\"{arch}\" {{\n{source}\n}}\n");
    let obj_file = atma::asm::assemble_source(&obj_source)
        .expect(&format!("Failed to assemble {source:?} for {arch}"));
    assert_eq!(obj_file.chunks.len(), 1);
    let obj_chunk = &obj_file.chunks[0];
    assert!(obj_chunk.patches.is_empty());
    let obj_data: &[u8] = &*obj_chunk.data;

    let mut rom_data = vec![0u8; obj_data.len().next_power_of_two()];
    rom_data[..obj_data.len()].copy_from_slice(obj_data);
    let rom_bus = atma::bus::new_rom_bus(rom_data.into_boxed_slice());
    let instruction = atma::dis::spc700::Instruction::decode(&*rom_bus, 0);
    let disassembled = instruction.format(&*rom_bus, 0);

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
fn assemble_call_instructions() {
    assert_asm("CALL !$1234", &[0x3f, 0x34, 0x12]);
    assert_asm("PCALL $ff37", &[0x4f, 0x37]);
}

#[test]
fn assemble_flag_instructions() {
    assert_asm("CLRC", &[0x60]);
    assert_asm("CLRP", &[0x20]);
    assert_asm("CLRV", &[0xe0]);
    assert_asm("DI", &[0xc0]);
    assert_asm("EI", &[0xa0]);
    assert_asm("NOTC", &[0xed]);
    assert_asm("SETC", &[0x80]);
    assert_asm("SETP", &[0x40]);
}

#[test]
fn assemble_inc_dec_instructions() {
    assert_asm("DEC A", &[0x9c]);
    assert_asm("DEC X", &[0x1d]);
    assert_asm("DEC Y", &[0xdc]);
    assert_asm("DEC $12", &[0x8b, 0x12]);
    assert_asm("DEC $34 + X", &[0x9b, 0x34]);
    assert_asm("DEC !$1234", &[0x8c, 0x34, 0x12]);
    assert_asm("DECW $56", &[0x1a, 0x56]);
    assert_asm("INC A", &[0xbc]);
    assert_asm("INC X", &[0x3d]);
    assert_asm("INC Y", &[0xfc]);
    assert_asm("INC $12", &[0xab, 0x12]);
    assert_asm("INC $34 + X", &[0xbb, 0x34]);
    assert_asm("INC !$1234", &[0xac, 0x34, 0x12]);
    assert_asm("INCW $56", &[0x3a, 0x56]);
}

#[test]
fn assemble_jump_instructions() {
    assert_asm("JMP !$1234", &[0x5f, 0x34, 0x12]);
    assert_asm("JMP [!$1234 + X]", &[0x1f, 0x34, 0x12]);
}

#[test]
fn assemble_miscellaneous_instructions() {
    assert_asm("BRK", &[0x0f]);
    assert_asm("DAA A", &[0xdf]);
    assert_asm("DAS A", &[0xbe]);
    assert_asm("DIV YA, X", &[0x9e]);
    assert_asm("MUL YA", &[0xcf]);
    assert_asm("NOP", &[0x00]);
    assert_asm("RET", &[0x6f]);
    assert_asm("RETI", &[0x7f]);
    assert_asm("SLEEP", &[0xef]);
    assert_asm("STOP", &[0xff]);
    assert_asm("XCN A", &[0x9f]);
}

#[test]
fn assemble_move_instructions() {
    assert_asm("MOV A, #$00", &[0xe8, 0x00]);
    assert_asm("MOV A, (X)", &[0xe6]);
    assert_asm("MOV A, (X)+", &[0xbf]);
    assert_asm("MOV A, $f5", &[0xe4, 0xf5]);
    assert_asm("MOV A, $f5 + X", &[0xf4, 0xf5]);
    assert_asm("MOV A, !$1234", &[0xe5, 0x34, 0x12]);
    assert_asm("MOV A, !$1234 + X", &[0xf5, 0x34, 0x12]);
    assert_asm("MOV A, !$1234 + Y", &[0xf6, 0x34, 0x12]);
    assert_asm("MOV A, [$cd + X]", &[0xe7, 0xcd]);
    assert_asm("MOV A, [$cd] + Y", &[0xf7, 0xcd]);
    assert_asm("MOV X, #$ef", &[0xcd, 0xef]);
    assert_asm("MOV X, $f4", &[0xf8, 0xf4]);
    assert_asm("MOV X, $f4 + Y", &[0xf9, 0xf4]);
    assert_asm("MOV X, !$1234", &[0xe9, 0x34, 0x12]);
    assert_asm("MOV Y, #$ef", &[0x8d, 0xef]);
    assert_asm("MOV Y, $f4", &[0xeb, 0xf4]);
    assert_asm("MOV Y, $f4 + X", &[0xfb, 0xf4]);
    assert_asm("MOV Y, !$1234", &[0xec, 0x34, 0x12]);
    assert_asm("MOV (X), A", &[0xc6]);
    assert_asm("MOV (X)+, A", &[0xaf]);
    assert_asm("MOV $fe, A", &[0xc4, 0xfe]);
    assert_asm("MOV $fe + X, A", &[0xd4, 0xfe]);
    assert_asm("MOV !$1234, A", &[0xc5, 0x34, 0x12]);
    assert_asm("MOV !$1234 + X, A", &[0xd5, 0x34, 0x12]);
    assert_asm("MOV !$1234 + Y, A", &[0xd6, 0x34, 0x12]);
    assert_asm("MOV [$fe + X], A", &[0xc7, 0xfe]);
    assert_asm("MOV [$fe] + Y, A", &[0xd7, 0xfe]);
    assert_asm("MOV $f4, X", &[0xd8, 0xf4]);
    assert_asm("MOV $f4 + Y, X", &[0xd9, 0xf4]);
    assert_asm("MOV !$1234, X", &[0xc9, 0x34, 0x12]);
    assert_asm("MOV $f4, Y", &[0xcb, 0xf4]);
    assert_asm("MOV $f4 + X, Y", &[0xdb, 0xf4]);
    assert_asm("MOV !$1234, Y", &[0xcc, 0x34, 0x12]);
    assert_asm("MOV A, X", &[0x7d]);
    assert_asm("MOV A, Y", &[0xdd]);
    assert_asm("MOV X, A", &[0x5d]);
    assert_asm("MOV Y, A", &[0xfd]);
    assert_asm("MOV X, SP", &[0x9d]);
    assert_asm("MOV SP, X", &[0xbd]);
    assert_asm("MOV $f4, $bb", &[0xfa, 0xbb, 0xf4]);
    assert_asm("MOV $f4, #$aa", &[0x8f, 0xaa, 0xf4]);
}

#[test]
fn assemble_push_pop_instructions() {
    assert_asm("PUSH A", &[0x2d]);
    assert_asm("PUSH X", &[0x4d]);
    assert_asm("PUSH Y", &[0x6d]);
    assert_asm("PUSH PSW", &[0x0d]);
    assert_asm("POP A", &[0xae]);
    assert_asm("POP X", &[0xce]);
    assert_asm("POP Y", &[0xee]);
    assert_asm("POP PSW", &[0x8e]);
}

#[test]
fn assemble_rotate_instructions() {
    assert_asm("ROL A", &[0x3c]);
    assert_asm("ROL $12", &[0x2b, 0x12]);
    assert_asm("ROL $34 + X", &[0x3b, 0x34]);
    assert_asm("ROL !$1234", &[0x2c, 0x34, 0x12]);
    assert_asm("ROR A", &[0x7c]);
    assert_asm("ROR $12", &[0x6b, 0x12]);
    assert_asm("ROR $34 + X", &[0x7b, 0x34]);
    assert_asm("ROR !$1234", &[0x6c, 0x34, 0x12]);
}

#[test]
fn assemble_shift_instructions() {
    assert_asm("ASL A", &[0x1c]);
    assert_asm("ASL $12", &[0x0b, 0x12]);
    assert_asm("ASL $34 + X", &[0x1b, 0x34]);
    assert_asm("ASL !$1234", &[0x0c, 0x34, 0x12]);
    assert_asm("LSR A", &[0x5c]);
    assert_asm("LSR $12", &[0x4b, 0x12]);
    assert_asm("LSR $34 + X", &[0x5b, 0x34]);
    assert_asm("LSR !$1234", &[0x4c, 0x34, 0x12]);
}

//===========================================================================//
