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
fn assemble_adc_instructions() {
    assert_asm_dis("ADC A, #$12", &[0x88, 0x12]);
    assert_asm_dis("ADC A, (X)", &[0x86]);
    assert_asm_dis("ADC A, $34", &[0x84, 0x34]);
    assert_asm_dis("ADC A, $56 + X", &[0x94, 0x56]);
    assert_asm_dis("ADC A, !$1234", &[0x85, 0x34, 0x12]);
    assert_asm_dis("ADC A, !$1234 + X", &[0x95, 0x34, 0x12]);
    assert_asm_dis("ADC A, !$1234 + Y", &[0x96, 0x34, 0x12]);
    assert_asm_dis("ADC A, [$cd + X]", &[0x87, 0xcd]);
    assert_asm_dis("ADC A, [$ab] + Y", &[0x97, 0xab]);
    assert_asm_dis("ADC (X), (Y)", &[0x99]);
    assert_asm_dis("ADC $12, $34", &[0x89, 0x34, 0x12]);
    assert_asm_dis("ADC $56, #$78", &[0x98, 0x78, 0x56]);
}

#[test]
fn assemble_and_instructions() {
    assert_asm_dis("AND A, #$12", &[0x28, 0x12]);
    assert_asm_dis("AND A, (X)", &[0x26]);
    assert_asm_dis("AND A, $34", &[0x24, 0x34]);
    assert_asm_dis("AND A, $56 + X", &[0x34, 0x56]);
    assert_asm_dis("AND A, !$1234", &[0x25, 0x34, 0x12]);
    assert_asm_dis("AND A, !$1234 + X", &[0x35, 0x34, 0x12]);
    assert_asm_dis("AND A, !$1234 + Y", &[0x36, 0x34, 0x12]);
    assert_asm_dis("AND A, [$cd + X]", &[0x27, 0xcd]);
    assert_asm_dis("AND A, [$ab] + Y", &[0x37, 0xab]);
    assert_asm_dis("AND (X), (Y)", &[0x39]);
    assert_asm_dis("AND $12, $34", &[0x29, 0x34, 0x12]);
    assert_asm_dis("AND $56, #$78", &[0x38, 0x78, 0x56]);
}

#[test]
fn assemble_bit_instructions() {
    assert_asm_dis("AND1 C, $1000, 0", &[0x4a, 0x00, 0x10]);
    assert_asm_dis("AND1 C, /$0000, 5", &[0x6a, 0x00, 0xa0]);
    assert_asm_dis("EOR1 C, $0123, 2", &[0x8a, 0x23, 0x41]);
    assert_asm_dis("MOV1 C, $1fff, 1", &[0xaa, 0xff, 0x3f]);
    assert_asm_dis("MOV1 $0012, 7, C", &[0xca, 0x12, 0xe0]);
    assert_asm_dis("NOT1 $1234, 6", &[0xea, 0x34, 0xd2]);
    assert_asm_dis("OR1 C, $1001, 4", &[0x0a, 0x01, 0x90]);
    assert_asm_dis("OR1 C, /$0002, 3", &[0x2a, 0x02, 0x60]);
}

#[test]
fn assemble_bbc_instructions() {
    assert_asm_dis("BBC $12, 0, $0034", &[0x13, 0x12, 0x31]);
    assert_asm_dis("BBC $56, 1, $0078", &[0x33, 0x56, 0x75]);
    assert_asm_dis("BBC $9a, 2, $ffff", &[0x53, 0x9a, 0xfc]);
    assert_asm_dis("BBC $bc, 3, $0000", &[0x73, 0xbc, 0xfd]);
    assert_asm_dis("BBC $12, 4, $0034", &[0x93, 0x12, 0x31]);
    assert_asm_dis("BBC $56, 5, $0078", &[0xb3, 0x56, 0x75]);
    assert_asm_dis("BBC $9a, 6, $ffff", &[0xd3, 0x9a, 0xfc]);
    assert_asm_dis("BBC $bc, 7, $0000", &[0xf3, 0xbc, 0xfd]);
}

#[test]
fn assemble_bbs_instructions() {
    assert_asm_dis("BBS $12, 0, $0034", &[0x03, 0x12, 0x31]);
    assert_asm_dis("BBS $56, 1, $0078", &[0x23, 0x56, 0x75]);
    assert_asm_dis("BBS $9a, 2, $ffff", &[0x43, 0x9a, 0xfc]);
    assert_asm_dis("BBS $bc, 3, $0000", &[0x63, 0xbc, 0xfd]);
    assert_asm_dis("BBS $12, 4, $0034", &[0x83, 0x12, 0x31]);
    assert_asm_dis("BBS $56, 5, $0078", &[0xa3, 0x56, 0x75]);
    assert_asm_dis("BBS $9a, 6, $ffff", &[0xc3, 0x9a, 0xfc]);
    assert_asm_dis("BBS $bc, 7, $0000", &[0xe3, 0xbc, 0xfd]);
}

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
    assert_asm_dis("CBNE $12, $0037", &[0x2e, 0x12, 0x34]);
    assert_asm_dis("CBNE $56 + X, $007b", &[0xde, 0x56, 0x78]);
    assert_asm_dis("DBNZ Y, $0014", &[0xfe, 0x12]);
    assert_asm_dis("DBNZ $34, $0015", &[0x6e, 0x34, 0x12]);
}

#[test]
fn assemble_call_instructions() {
    assert_asm_dis("CALL !$1234", &[0x3f, 0x34, 0x12]);
    assert_asm_dis("PCALL $ff37", &[0x4f, 0x37]);
    assert_asm_dis("TCALL 0", &[0x01]);
    assert_asm_dis("TCALL 1", &[0x11]);
    assert_asm_dis("TCALL 2", &[0x21]);
    assert_asm_dis("TCALL 3", &[0x31]);
    assert_asm_dis("TCALL 4", &[0x41]);
    assert_asm_dis("TCALL 5", &[0x51]);
    assert_asm_dis("TCALL 6", &[0x61]);
    assert_asm_dis("TCALL 7", &[0x71]);
    assert_asm_dis("TCALL 8", &[0x81]);
    assert_asm_dis("TCALL 9", &[0x91]);
    assert_asm_dis("TCALL 10", &[0xa1]);
    assert_asm_dis("TCALL 11", &[0xb1]);
    assert_asm_dis("TCALL 12", &[0xc1]);
    assert_asm_dis("TCALL 13", &[0xd1]);
    assert_asm_dis("TCALL 14", &[0xe1]);
    assert_asm_dis("TCALL 15", &[0xf1]);
}

#[test]
fn assemble_clr1_instructions() {
    assert_asm_dis("CLR1 $12, 0", &[0x12, 0x12]);
    assert_asm_dis("CLR1 $56, 1", &[0x32, 0x56]);
    assert_asm_dis("CLR1 $9a, 2", &[0x52, 0x9a]);
    assert_asm_dis("CLR1 $bc, 3", &[0x72, 0xbc]);
    assert_asm_dis("CLR1 $12, 4", &[0x92, 0x12]);
    assert_asm_dis("CLR1 $56, 5", &[0xb2, 0x56]);
    assert_asm_dis("CLR1 $9a, 6", &[0xd2, 0x9a]);
    assert_asm_dis("CLR1 $bc, 7", &[0xf2, 0xbc]);
}

#[test]
fn assemble_cmp_instructions() {
    assert_asm_dis("CMP A, #$12", &[0x68, 0x12]);
    assert_asm_dis("CMP A, (X)", &[0x66]);
    assert_asm_dis("CMP A, $34", &[0x64, 0x34]);
    assert_asm_dis("CMP A, $56 + X", &[0x74, 0x56]);
    assert_asm_dis("CMP A, !$1234", &[0x65, 0x34, 0x12]);
    assert_asm_dis("CMP A, !$1234 + X", &[0x75, 0x34, 0x12]);
    assert_asm_dis("CMP A, !$1234 + Y", &[0x76, 0x34, 0x12]);
    assert_asm_dis("CMP A, [$cd + X]", &[0x67, 0xcd]);
    assert_asm_dis("CMP A, [$ab] + Y", &[0x77, 0xab]);
    assert_asm_dis("CMP (X), (Y)", &[0x79]);
    assert_asm_dis("CMP $12, $34", &[0x69, 0x34, 0x12]);
    assert_asm_dis("CMP $56, #$78", &[0x78, 0x78, 0x56]);
    assert_asm_dis("CMP X, #$12", &[0xc8, 0x12]);
    assert_asm_dis("CMP X, $34", &[0x3e, 0x34]);
    assert_asm_dis("CMP X, !$1234", &[0x1e, 0x34, 0x12]);
    assert_asm_dis("CMP Y, #$12", &[0xad, 0x12]);
    assert_asm_dis("CMP Y, $34", &[0x7e, 0x34]);
    assert_asm_dis("CMP Y, !$1234", &[0x5e, 0x34, 0x12]);
}

#[test]
fn assemble_eor_instructions() {
    assert_asm_dis("EOR A, #$12", &[0x48, 0x12]);
    assert_asm_dis("EOR A, (X)", &[0x46]);
    assert_asm_dis("EOR A, $34", &[0x44, 0x34]);
    assert_asm_dis("EOR A, $56 + X", &[0x54, 0x56]);
    assert_asm_dis("EOR A, !$1234", &[0x45, 0x34, 0x12]);
    assert_asm_dis("EOR A, !$1234 + X", &[0x55, 0x34, 0x12]);
    assert_asm_dis("EOR A, !$1234 + Y", &[0x56, 0x34, 0x12]);
    assert_asm_dis("EOR A, [$cd + X]", &[0x47, 0xcd]);
    assert_asm_dis("EOR A, [$ab] + Y", &[0x57, 0xab]);
    assert_asm_dis("EOR (X), (Y)", &[0x59]);
    assert_asm_dis("EOR $12, $34", &[0x49, 0x34, 0x12]);
    assert_asm_dis("EOR $56, #$78", &[0x58, 0x78, 0x56]);
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
    assert_asm_dis("INC A", &[0xbc]);
    assert_asm_dis("INC X", &[0x3d]);
    assert_asm_dis("INC Y", &[0xfc]);
    assert_asm_dis("INC $12", &[0xab, 0x12]);
    assert_asm_dis("INC $34 + X", &[0xbb, 0x34]);
    assert_asm_dis("INC !$1234", &[0xac, 0x34, 0x12]);
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
fn assemble_or_instructions() {
    assert_asm_dis("OR A, #$12", &[0x08, 0x12]);
    assert_asm_dis("OR A, (X)", &[0x06]);
    assert_asm_dis("OR A, $34", &[0x04, 0x34]);
    assert_asm_dis("OR A, $56 + X", &[0x14, 0x56]);
    assert_asm_dis("OR A, !$1234", &[0x05, 0x34, 0x12]);
    assert_asm_dis("OR A, !$1234 + X", &[0x15, 0x34, 0x12]);
    assert_asm_dis("OR A, !$1234 + Y", &[0x16, 0x34, 0x12]);
    assert_asm_dis("OR A, [$cd + X]", &[0x07, 0xcd]);
    assert_asm_dis("OR A, [$ab] + Y", &[0x17, 0xab]);
    assert_asm_dis("OR (X), (Y)", &[0x19]);
    assert_asm_dis("OR $12, $34", &[0x09, 0x34, 0x12]);
    assert_asm_dis("OR $56, #$78", &[0x18, 0x78, 0x56]);
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
fn assemble_sbc_instructions() {
    assert_asm_dis("SBC A, #$12", &[0xa8, 0x12]);
    assert_asm_dis("SBC A, (X)", &[0xa6]);
    assert_asm_dis("SBC A, $34", &[0xa4, 0x34]);
    assert_asm_dis("SBC A, $56 + X", &[0xb4, 0x56]);
    assert_asm_dis("SBC A, !$1234", &[0xa5, 0x34, 0x12]);
    assert_asm_dis("SBC A, !$1234 + X", &[0xb5, 0x34, 0x12]);
    assert_asm_dis("SBC A, !$1234 + Y", &[0xb6, 0x34, 0x12]);
    assert_asm_dis("SBC A, [$cd + X]", &[0xa7, 0xcd]);
    assert_asm_dis("SBC A, [$ab] + Y", &[0xb7, 0xab]);
    assert_asm_dis("SBC (X), (Y)", &[0xb9]);
    assert_asm_dis("SBC $12, $34", &[0xa9, 0x34, 0x12]);
    assert_asm_dis("SBC $56, #$78", &[0xb8, 0x78, 0x56]);
}

#[test]
fn assemble_set1_instructions() {
    assert_asm_dis("SET1 $12, 0", &[0x02, 0x12]);
    assert_asm_dis("SET1 $56, 1", &[0x22, 0x56]);
    assert_asm_dis("SET1 $9a, 2", &[0x42, 0x9a]);
    assert_asm_dis("SET1 $bc, 3", &[0x62, 0xbc]);
    assert_asm_dis("SET1 $12, 4", &[0x82, 0x12]);
    assert_asm_dis("SET1 $56, 5", &[0xa2, 0x56]);
    assert_asm_dis("SET1 $9a, 6", &[0xc2, 0x9a]);
    assert_asm_dis("SET1 $bc, 7", &[0xe2, 0xbc]);
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

#[test]
fn assemble_tclr1_tset1_instructions() {
    assert_asm_dis("TCLR1 !$1234", &[0x4e, 0x34, 0x12]);
    assert_asm_dis("TSET1 !$5678", &[0x0e, 0x78, 0x56]);
}

#[test]
fn assemble_word_instructions() {
    assert_asm_dis("ADDW YA, $12", &[0x7a, 0x12]);
    assert_asm_dis("CMPW YA, $34", &[0x5a, 0x34]);
    assert_asm_dis("DECW $56", &[0x1a, 0x56]);
    assert_asm_dis("INCW $78", &[0x3a, 0x78]);
    assert_asm_dis("MOVW YA, $9a", &[0xba, 0x9a]);
    assert_asm_dis("MOVW $bc, YA", &[0xda, 0xbc]);
    assert_asm_dis("SUBW YA, $de", &[0x9a, 0xde]);
}

//===========================================================================//
