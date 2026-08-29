use atma;
use std::rc::Rc;

//===========================================================================//

const FLAG_NONE: u8 = 0x00;
const FLAG_M: u8 = 0x20;
const FLAG_X: u8 = 0x10;

//===========================================================================//

fn assemble(source: &str) -> Vec<u8> {
    let arch = "65C816";
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

fn disassemble(binary: &[u8], flags: u8) -> String {
    let mut rom_data = vec![0u8; binary.len().next_power_of_two()];
    rom_data[..binary.len()].copy_from_slice(binary);
    let rom_bus = atma::bus::new_rom_bus(rom_data.into_boxed_slice());
    let pc = 0;
    let flag_m = flags & FLAG_M != 0;
    let flag_x = flags & FLAG_X != 0;
    let dpr = 0x0000u16;
    let dbr = 0x00u8;
    let instruction =
        atma::dis::w65c816::Instruction::decode(&*rom_bus, pc, flag_m, flag_x);
    instruction.format(&*rom_bus, pc, dpr, dbr)
}

fn assert_asm_dis(source: &str, binary: &[u8], flags: u8) {
    let obj_data = assemble(source);
    let disassembled = disassemble(&obj_data, flags);
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
    assert_asm_dis("BIT #$12", &[0x89, 0x12], FLAG_M);
    assert_asm_dis("BIT ##$1234", &[0x89, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("BIT !$1234", &[0x2c, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("BIT !$1234, X", &[0x3c, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("BIT $12", &[0x24, 0x12], FLAG_NONE);
    assert_asm_dis("BIT $12, X", &[0x34, 0x12], FLAG_NONE);
}

#[test]
fn assemble_branch_instructions() {
    assert_asm_dis("BCC $0012", &[0x90, 0x10], FLAG_NONE);
    assert_asm_dis("BCS $0010", &[0xb0, 0x0e], FLAG_NONE);
    assert_asm_dis("BEQ $ff82", &[0xf0, 0x80], FLAG_NONE);
    assert_asm_dis("BMI $fff2", &[0x30, 0xf0], FLAG_NONE);
    assert_asm_dis("BNE $ff83", &[0xd0, 0x81], FLAG_NONE);
    assert_asm_dis("BPL $0081", &[0x10, 0x7f], FLAG_NONE);
    assert_asm_dis("BRA $0012", &[0x80, 0x10], FLAG_NONE);
    assert_asm_dis("BRL $0200", &[0x82, 0xfd, 0x01], FLAG_NONE);
    assert_asm_dis("BVC $0000", &[0x50, 0xfe], FLAG_NONE);
    assert_asm_dis("BVS $ffff", &[0x70, 0xfd], FLAG_NONE);
}

#[test]
fn assemble_dec_instructions() {
    assert_asm_dis("DEC A", &[0x3a], FLAG_NONE);
    assert_asm_dis("DEC !$1234", &[0xce, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("DEC !$1234, X", &[0xde, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("DEC $12", &[0xc6, 0x12], FLAG_NONE);
    assert_asm_dis("DEC $12, X", &[0xd6, 0x12], FLAG_NONE);
    assert_asm_dis("DEX", &[0xca], FLAG_NONE);
    assert_asm_dis("DEY", &[0x88], FLAG_NONE);
}

#[test]
fn assemble_flag_instructions() {
    assert_asm_dis("CLC", &[0x18], FLAG_NONE);
    assert_asm_dis("CLD", &[0xd8], FLAG_NONE);
    assert_asm_dis("CLI", &[0x58], FLAG_NONE);
    assert_asm_dis("CLV", &[0xb8], FLAG_NONE);
    assert_asm_dis("REP #$09", &[0xc2, 0x09], FLAG_NONE);
    assert_asm_dis("SEC", &[0x38], FLAG_NONE);
    assert_asm_dis("SED", &[0xf8], FLAG_NONE);
    assert_asm_dis("SEI", &[0x78], FLAG_NONE);
    assert_asm_dis("SEP #$30", &[0xe2, 0x30], FLAG_NONE);
    assert_asm_dis("XCE", &[0xfb], FLAG_NONE);
}

#[test]
fn assemble_inc_instructions() {
    assert_asm_dis("INC A", &[0x1a], FLAG_NONE);
    assert_asm_dis("INC !$1234", &[0xee, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("INC !$1234, X", &[0xfe, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("INC $12", &[0xe6, 0x12], FLAG_NONE);
    assert_asm_dis("INC $12, X", &[0xf6, 0x12], FLAG_NONE);
    assert_asm_dis("INX", &[0xe8], FLAG_NONE);
    assert_asm_dis("INY", &[0xc8], FLAG_NONE);
}

#[test]
fn assemble_jump_instructions() {
    assert_asm_dis("JMP !$1234", &[0x4c, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("JMP (!$1234)", &[0x6c, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("JMP (!$1234, X)", &[0x7c, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("JML [!$1234]", &[0xdc, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("JML !!$123456", &[0x5c, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("JSR !$1234", &[0x20, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("JSR (!$1234, X)", &[0xfc, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("JSL !!$123456", &[0x22, 0x56, 0x34, 0x12], FLAG_NONE);
}

#[test]
fn assemble_lda_instructions() {
    assert_asm_dis("LDA #$12", &[0xa9, 0x12], FLAG_M);
    assert_asm_dis("LDA ##$1234", &[0xa9, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDA !$1234", &[0xad, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDA !$1234, X", &[0xbd, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDA !$1234, Y", &[0xb9, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDA $12", &[0xa5, 0x12], FLAG_NONE);
    assert_asm_dis("LDA $12, X", &[0xb5, 0x12], FLAG_NONE);
    assert_asm_dis("LDA ($12)", &[0xb2, 0x12], FLAG_NONE);
    assert_asm_dis("LDA ($12, X)", &[0xa1, 0x12], FLAG_NONE);
    assert_asm_dis("LDA ($12), Y", &[0xb1, 0x12], FLAG_NONE);
    assert_asm_dis("LDA [$12]", &[0xa7, 0x12], FLAG_NONE);
    assert_asm_dis("LDA [$12], Y", &[0xb7, 0x12], FLAG_NONE);
    assert_asm_dis("LDA !!$123456", &[0xaf, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDA !!$123456, X", &[0xbf, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDA $12, S", &[0xa3, 0x12], FLAG_NONE);
    assert_asm_dis("LDA ($12, S), Y", &[0xb3, 0x12], FLAG_NONE);
}

#[test]
fn assemble_ldx_instructions() {
    assert_asm_dis("LDX #$12", &[0xa2, 0x12], FLAG_X);
    assert_asm_dis("LDX ##$1234", &[0xa2, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDX !$1234", &[0xae, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDX !$1234, Y", &[0xbe, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDX $12", &[0xa6, 0x12], FLAG_NONE);
    assert_asm_dis("LDX $12, Y", &[0xb6, 0x12], FLAG_NONE);
}

#[test]
fn assemble_ldy_instructions() {
    assert_asm_dis("LDY #$12", &[0xa0, 0x12], FLAG_X);
    assert_asm_dis("LDY ##$1234", &[0xa0, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDY !$1234", &[0xac, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDY !$1234, X", &[0xbc, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("LDY $12", &[0xa4, 0x12], FLAG_NONE);
    assert_asm_dis("LDY $12, X", &[0xb4, 0x12], FLAG_NONE);
}

#[test]
fn assemble_misc_instructions() {
    assert_asm_dis("BRK #$12", &[0x00, 0x12], FLAG_NONE);
    assert_asm_dis("COP #$12", &[0x02, 0x12], FLAG_NONE);
    assert_asm_dis("NOP", &[0xea], FLAG_NONE);
    assert_asm_dis("RTI", &[0x40], FLAG_NONE);
    assert_asm_dis("RTL", &[0x6b], FLAG_NONE);
    assert_asm_dis("RTS", &[0x60], FLAG_NONE);
    assert_asm_dis("STP", &[0xdb], FLAG_NONE);
    assert_asm_dis("WAI", &[0xcb], FLAG_NONE);
    assert_asm_dis("WDM #$12", &[0x42, 0x12], FLAG_NONE);
    assert_asm_dis("XBA", &[0xeb], FLAG_NONE);
}

#[test]
fn assemble_move_instructions() {
    assert_asm_dis("MVN #$12, #$34", &[0x54, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("MVP #$12, #$34", &[0x44, 0x34, 0x12], FLAG_NONE);
}

#[test]
fn assemble_pull_instructions() {
    assert_asm_dis("PLA", &[0x68], FLAG_NONE);
    assert_asm_dis("PLB", &[0xab], FLAG_NONE);
    assert_asm_dis("PLD", &[0x2b], FLAG_NONE);
    assert_asm_dis("PLP", &[0x28], FLAG_NONE);
    assert_asm_dis("PLX", &[0xfa], FLAG_NONE);
    assert_asm_dis("PLY", &[0x7a], FLAG_NONE);
}

#[test]
fn assemble_push_instructions() {
    assert_asm_dis("PEA !$1234", &[0xf4, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("PEI ($12)", &[0xd4, 0x12], FLAG_NONE);
    assert_asm_dis("PER $8000", &[0x62, 0xfd, 0x7f], FLAG_NONE);
    assert_asm_dis("PHA", &[0x48], FLAG_NONE);
    assert_asm_dis("PHB", &[0x8b], FLAG_NONE);
    assert_asm_dis("PHD", &[0x0b], FLAG_NONE);
    assert_asm_dis("PHK", &[0x4b], FLAG_NONE);
    assert_asm_dis("PHP", &[0x08], FLAG_NONE);
    assert_asm_dis("PHX", &[0xda], FLAG_NONE);
    assert_asm_dis("PHY", &[0x5a], FLAG_NONE);
}

#[test]
fn assemble_sta_instructions() {
    assert_asm_dis("STA !$1234", &[0x8d, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STA !$1234, X", &[0x9d, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STA !$1234, Y", &[0x99, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STA $12", &[0x85, 0x12], FLAG_NONE);
    assert_asm_dis("STA $12, X", &[0x95, 0x12], FLAG_NONE);
    assert_asm_dis("STA ($12)", &[0x92, 0x12], FLAG_NONE);
    assert_asm_dis("STA ($12, X)", &[0x81, 0x12], FLAG_NONE);
    assert_asm_dis("STA ($12), Y", &[0x91, 0x12], FLAG_NONE);
    assert_asm_dis("STA [$12]", &[0x87, 0x12], FLAG_NONE);
    assert_asm_dis("STA [$12], Y", &[0x97, 0x12], FLAG_NONE);
    assert_asm_dis("STA !!$123456", &[0x8f, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STA !!$123456, X", &[0x9f, 0x56, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STA $12, S", &[0x83, 0x12], FLAG_NONE);
    assert_asm_dis("STA ($12, S), Y", &[0x93, 0x12], FLAG_NONE);
}

#[test]
fn assemble_stx_instructions() {
    assert_asm_dis("STX !$1234", &[0x8e, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STX $12", &[0x86, 0x12], FLAG_NONE);
    assert_asm_dis("STX $12, Y", &[0x96, 0x12], FLAG_NONE);
}

#[test]
fn assemble_sty_instructions() {
    assert_asm_dis("STY !$1234", &[0x8c, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STY $12", &[0x84, 0x12], FLAG_NONE);
    assert_asm_dis("STY $12, X", &[0x94, 0x12], FLAG_NONE);
}

#[test]
fn assemble_stz_instructions() {
    assert_asm_dis("STZ !$1234", &[0x9c, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STZ !$1234, X", &[0x9e, 0x34, 0x12], FLAG_NONE);
    assert_asm_dis("STZ $12", &[0x64, 0x12], FLAG_NONE);
    assert_asm_dis("STZ $12, X", &[0x74, 0x12], FLAG_NONE);
}

#[test]
fn assemble_transfer_instructions() {
    assert_asm_dis("TAX", &[0xaa], FLAG_NONE);
    assert_asm_dis("TAY", &[0xa8], FLAG_NONE);
    assert_asm_dis("TCD", &[0x5b], FLAG_NONE);
    assert_asm_dis("TCS", &[0x1b], FLAG_NONE);
    assert_asm_dis("TDC", &[0x7b], FLAG_NONE);
    assert_asm_dis("TSC", &[0x3b], FLAG_NONE);
    assert_asm_dis("TSX", &[0xba], FLAG_NONE);
    assert_asm_dis("TXA", &[0x8a], FLAG_NONE);
    assert_asm_dis("TXS", &[0x9a], FLAG_NONE);
    assert_asm_dis("TXY", &[0x9b], FLAG_NONE);
    assert_asm_dis("TYA", &[0x98], FLAG_NONE);
    assert_asm_dis("TYX", &[0xbb], FLAG_NONE);
}

//===========================================================================//
