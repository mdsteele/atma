use atma;

//===========================================================================//

const FLAG_NONE: u8 = 0x00;
const FLAG_M: u8 = 0x20;
const FLAG_X: u8 = 0x10;

//===========================================================================//

fn assert_asm(source: &str, binary: &[u8], flags: u8) {
    let arch = "65C816";
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
    let flag_m = flags & FLAG_M != 0;
    let flag_x = flags & FLAG_X != 0;
    let instruction =
        atma::dis::w65c816::Instruction::decode(&*rom_bus, 0, flag_m, flag_x);
    let disassembled = instruction.format(&*rom_bus, 0, 0x0000, 0x0000);

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
fn assemble_bit_instructions() {
    assert_asm("BIT #$12", &[0x89, 0x12], FLAG_M);
    assert_asm("BIT ##$1234", &[0x89, 0x34, 0x12], FLAG_NONE);
    assert_asm("BIT !$1234", &[0x2c, 0x34, 0x12], FLAG_NONE);
    assert_asm("BIT !$1234, X", &[0x3c, 0x34, 0x12], FLAG_NONE);
    assert_asm("BIT $12", &[0x24, 0x12], FLAG_NONE);
    assert_asm("BIT $12, X", &[0x34, 0x12], FLAG_NONE);
}

#[test]
fn assemble_dec_instructions() {
    assert_asm("DEC A", &[0x3a], FLAG_NONE);
    assert_asm("DEC !$1234", &[0xce, 0x34, 0x12], FLAG_NONE);
    assert_asm("DEC !$1234, X", &[0xde, 0x34, 0x12], FLAG_NONE);
    assert_asm("DEC $12", &[0xc6, 0x12], FLAG_NONE);
    assert_asm("DEC $12, X", &[0xd6, 0x12], FLAG_NONE);
    assert_asm("DEX", &[0xca], FLAG_NONE);
    assert_asm("DEY", &[0x88], FLAG_NONE);
}

#[test]
fn assemble_flag_instructions() {
    assert_asm("CLC", &[0x18], FLAG_NONE);
    assert_asm("CLD", &[0xd8], FLAG_NONE);
    assert_asm("CLI", &[0x58], FLAG_NONE);
    assert_asm("CLV", &[0xb8], FLAG_NONE);
    assert_asm("REP #$09", &[0xc2, 0x09], FLAG_NONE);
    assert_asm("SEC", &[0x38], FLAG_NONE);
    assert_asm("SED", &[0xf8], FLAG_NONE);
    assert_asm("SEI", &[0x78], FLAG_NONE);
    assert_asm("SEP #$30", &[0xe2, 0x30], FLAG_NONE);
    assert_asm("XCE", &[0xfb], FLAG_NONE);
}

#[test]
fn assemble_inc_instructions() {
    assert_asm("INC A", &[0x1a], FLAG_NONE);
    assert_asm("INC !$1234", &[0xee, 0x34, 0x12], FLAG_NONE);
    assert_asm("INC !$1234, X", &[0xfe, 0x34, 0x12], FLAG_NONE);
    assert_asm("INC $12", &[0xe6, 0x12], FLAG_NONE);
    assert_asm("INC $12, X", &[0xf6, 0x12], FLAG_NONE);
    assert_asm("INX", &[0xe8], FLAG_NONE);
    assert_asm("INY", &[0xc8], FLAG_NONE);
}

#[test]
fn assemble_jump_instructions() {
    assert_asm("JMP !$1234", &[0x4c, 0x34, 0x12], FLAG_NONE);
    assert_asm("JMP (!$1234)", &[0x6c, 0x34, 0x12], FLAG_NONE);
    assert_asm("JMP (!$1234, X)", &[0x7c, 0x34, 0x12], FLAG_NONE);
    assert_asm("JML [!$1234]", &[0xdc, 0x34, 0x12], FLAG_NONE);
    assert_asm("JML !!$123456", &[0x5c, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm("JSR !$1234", &[0x20, 0x34, 0x12], FLAG_NONE);
    assert_asm("JSR (!$1234, X)", &[0xfc, 0x34, 0x12], FLAG_NONE);
    assert_asm("JSL !!$123456", &[0x22, 0x56, 0x34, 0x12], FLAG_NONE);
}

#[test]
fn assemble_lda_instructions() {
    assert_asm("LDA #$12", &[0xa9, 0x12], FLAG_M);
    assert_asm("LDA ##$1234", &[0xa9, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDA !$1234", &[0xad, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDA !$1234, X", &[0xbd, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDA !$1234, Y", &[0xb9, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDA $12", &[0xa5, 0x12], FLAG_NONE);
    assert_asm("LDA $12, X", &[0xb5, 0x12], FLAG_NONE);
    assert_asm("LDA ($12)", &[0xb2, 0x12], FLAG_NONE);
    assert_asm("LDA ($12, X)", &[0xa1, 0x12], FLAG_NONE);
    assert_asm("LDA ($12), Y", &[0xb1, 0x12], FLAG_NONE);
    assert_asm("LDA [$12]", &[0xa7, 0x12], FLAG_NONE);
    assert_asm("LDA [$12], Y", &[0xb7, 0x12], FLAG_NONE);
    assert_asm("LDA !!$123456", &[0xaf, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDA !!$123456, X", &[0xbf, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDA $12, S", &[0xa3, 0x12], FLAG_NONE);
    assert_asm("LDA ($12, S), Y", &[0xb3, 0x12], FLAG_NONE);
}

#[test]
fn assemble_ldx_instructions() {
    assert_asm("LDX #$12", &[0xa2, 0x12], FLAG_X);
    assert_asm("LDX ##$1234", &[0xa2, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDX !$1234", &[0xae, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDX !$1234, Y", &[0xbe, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDX $12", &[0xa6, 0x12], FLAG_NONE);
    assert_asm("LDX $12, Y", &[0xb6, 0x12], FLAG_NONE);
}

#[test]
fn assemble_ldy_instructions() {
    assert_asm("LDY #$12", &[0xa0, 0x12], FLAG_X);
    assert_asm("LDY ##$1234", &[0xa0, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDY !$1234", &[0xac, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDY !$1234, X", &[0xbc, 0x34, 0x12], FLAG_NONE);
    assert_asm("LDY $12", &[0xa4, 0x12], FLAG_NONE);
    assert_asm("LDY $12, X", &[0xb4, 0x12], FLAG_NONE);
}

#[test]
fn assemble_misc_instructions() {
    assert_asm("BRK #$12", &[0x00, 0x12], FLAG_NONE);
    assert_asm("COP #$12", &[0x02, 0x12], FLAG_NONE);
    assert_asm("NOP", &[0xea], FLAG_NONE);
    assert_asm("RTI", &[0x40], FLAG_NONE);
    assert_asm("RTL", &[0x6b], FLAG_NONE);
    assert_asm("RTS", &[0x60], FLAG_NONE);
    assert_asm("STP", &[0xdb], FLAG_NONE);
    assert_asm("WAI", &[0xcb], FLAG_NONE);
    assert_asm("WDM #$12", &[0x42, 0x12], FLAG_NONE);
    assert_asm("XBA", &[0xeb], FLAG_NONE);
}

#[test]
fn assemble_move_instructions() {
    assert_asm("MVN #$12, #$34", &[0x54, 0x34, 0x12], FLAG_NONE);
    assert_asm("MVP #$12, #$34", &[0x44, 0x34, 0x12], FLAG_NONE);
}

#[test]
fn assemble_pull_instructions() {
    assert_asm("PLA", &[0x68], FLAG_NONE);
    assert_asm("PLB", &[0xab], FLAG_NONE);
    assert_asm("PLD", &[0x2b], FLAG_NONE);
    assert_asm("PLP", &[0x28], FLAG_NONE);
    assert_asm("PLX", &[0xfa], FLAG_NONE);
    assert_asm("PLY", &[0x7a], FLAG_NONE);
}

#[test]
fn assemble_push_instructions() {
    assert_asm("PEA !$1234", &[0xf4, 0x34, 0x12], FLAG_NONE);
    assert_asm("PEI ($12)", &[0xd4, 0x12], FLAG_NONE);
    // TODO: assert_asm("PER $ffff", &[0x62, 0xfc, 0xff], FLAG_NONE);
    assert_asm("PHA", &[0x48], FLAG_NONE);
    assert_asm("PHB", &[0x8b], FLAG_NONE);
    assert_asm("PHD", &[0x0b], FLAG_NONE);
    assert_asm("PHK", &[0x4b], FLAG_NONE);
    assert_asm("PHP", &[0x08], FLAG_NONE);
    assert_asm("PHX", &[0xda], FLAG_NONE);
    assert_asm("PHY", &[0x5a], FLAG_NONE);
}

#[test]
fn assemble_sta_instructions() {
    assert_asm("STA !$1234", &[0x8d, 0x34, 0x12], FLAG_NONE);
    assert_asm("STA !$1234, X", &[0x9d, 0x34, 0x12], FLAG_NONE);
    assert_asm("STA !$1234, Y", &[0x99, 0x34, 0x12], FLAG_NONE);
    assert_asm("STA $12", &[0x85, 0x12], FLAG_NONE);
    assert_asm("STA $12, X", &[0x95, 0x12], FLAG_NONE);
    assert_asm("STA ($12)", &[0x92, 0x12], FLAG_NONE);
    assert_asm("STA ($12, X)", &[0x81, 0x12], FLAG_NONE);
    assert_asm("STA ($12), Y", &[0x91, 0x12], FLAG_NONE);
    assert_asm("STA [$12]", &[0x87, 0x12], FLAG_NONE);
    assert_asm("STA [$12], Y", &[0x97, 0x12], FLAG_NONE);
    assert_asm("STA !!$123456", &[0x8f, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm("STA !!$123456, X", &[0x9f, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm("STA $12, S", &[0x83, 0x12], FLAG_NONE);
    assert_asm("STA ($12, S), Y", &[0x93, 0x12], FLAG_NONE);
}

#[test]
fn assemble_stx_instructions() {
    assert_asm("STX !$1234", &[0x8e, 0x34, 0x12], FLAG_NONE);
    assert_asm("STX $12", &[0x86, 0x12], FLAG_NONE);
    assert_asm("STX $12, Y", &[0x96, 0x12], FLAG_NONE);
}

#[test]
fn assemble_sty_instructions() {
    assert_asm("STY !$1234", &[0x8c, 0x34, 0x12], FLAG_NONE);
    assert_asm("STY $12", &[0x84, 0x12], FLAG_NONE);
    assert_asm("STY $12, X", &[0x94, 0x12], FLAG_NONE);
}

#[test]
fn assemble_stz_instructions() {
    assert_asm("STZ !$1234", &[0x9c, 0x34, 0x12], FLAG_NONE);
    assert_asm("STZ !$1234, X", &[0x9e, 0x34, 0x12], FLAG_NONE);
    assert_asm("STZ $12", &[0x64, 0x12], FLAG_NONE);
    assert_asm("STZ $12, X", &[0x74, 0x12], FLAG_NONE);
}

#[test]
fn assemble_transfer_instructions() {
    assert_asm("TAX", &[0xaa], FLAG_NONE);
    assert_asm("TAY", &[0xa8], FLAG_NONE);
    assert_asm("TCD", &[0x5b], FLAG_NONE);
    assert_asm("TCS", &[0x1b], FLAG_NONE);
    assert_asm("TDC", &[0x7b], FLAG_NONE);
    assert_asm("TSC", &[0x3b], FLAG_NONE);
    assert_asm("TSX", &[0xba], FLAG_NONE);
    assert_asm("TXA", &[0x8a], FLAG_NONE);
    assert_asm("TXS", &[0x9a], FLAG_NONE);
    assert_asm("TXY", &[0x9b], FLAG_NONE);
    assert_asm("TYA", &[0x98], FLAG_NONE);
    assert_asm("TYX", &[0xbb], FLAG_NONE);
}

//===========================================================================//
