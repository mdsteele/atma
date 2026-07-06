use atma;
use std::rc::Rc;

//===========================================================================//

fn assert_asm(source: &str, binary: &[u8]) {
    let arch = "6502";
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
    let instruction = atma::dis::mos6502::Instruction::decode(&*rom_bus, pc);
    let disassembled = instruction.format(&*rom_bus, pc);

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
    assert_asm("ADC #$12", &[0x69, 0x12]);
    assert_asm("ADC !$1234", &[0x6d, 0x34, 0x12]);
    assert_asm("ADC !$1234, X", &[0x7d, 0x34, 0x12]);
    assert_asm("ADC !$1234, Y", &[0x79, 0x34, 0x12]);
    assert_asm("ADC $12", &[0x65, 0x12]);
    assert_asm("ADC $12, X", &[0x75, 0x12]);
    assert_asm("ADC ($12, X)", &[0x61, 0x12]);
    assert_asm("ADC ($12), Y", &[0x71, 0x12]);
}

#[test]
fn assemble_and_instructions() {
    assert_asm("AND #$12", &[0x29, 0x12]);
    assert_asm("AND !$1234", &[0x2d, 0x34, 0x12]);
    assert_asm("AND !$1234, X", &[0x3d, 0x34, 0x12]);
    assert_asm("AND !$1234, Y", &[0x39, 0x34, 0x12]);
    assert_asm("AND $12", &[0x25, 0x12]);
    assert_asm("AND $12, X", &[0x35, 0x12]);
    assert_asm("AND ($12, X)", &[0x21, 0x12]);
    assert_asm("AND ($12), Y", &[0x31, 0x12]);
}

#[test]
fn assemble_asl_instructions() {
    assert_asm("ASL A", &[0x0a]);
    assert_asm("ASL !$1234", &[0x0e, 0x34, 0x12]);
    assert_asm("ASL !$1234, X", &[0x1e, 0x34, 0x12]);
    assert_asm("ASL $12", &[0x06, 0x12]);
    assert_asm("ASL $12, X", &[0x16, 0x12]);
}

#[test]
fn assemble_bit_instructions() {
    assert_asm("BIT !$1234", &[0x2c, 0x34, 0x12]);
    assert_asm("BIT $12", &[0x24, 0x12]);
}

#[test]
fn assemble_dec_instructions() {
    assert_asm("DEC !$1234", &[0xce, 0x34, 0x12]);
    assert_asm("DEC !$1234, X", &[0xde, 0x34, 0x12]);
    assert_asm("DEC $12", &[0xc6, 0x12]);
    assert_asm("DEC $12, X", &[0xd6, 0x12]);
    assert_asm("DEX", &[0xca]);
    assert_asm("DEY", &[0x88]);
}

#[test]
fn assemble_eor_instructions() {
    assert_asm("EOR #$12", &[0x49, 0x12]);
    assert_asm("EOR !$1234", &[0x4d, 0x34, 0x12]);
    assert_asm("EOR !$1234, X", &[0x5d, 0x34, 0x12]);
    assert_asm("EOR !$1234, Y", &[0x59, 0x34, 0x12]);
    assert_asm("EOR $12", &[0x45, 0x12]);
    assert_asm("EOR $12, X", &[0x55, 0x12]);
    assert_asm("EOR ($12, X)", &[0x41, 0x12]);
    assert_asm("EOR ($12), Y", &[0x51, 0x12]);
}

#[test]
fn assemble_flag_instructions() {
    assert_asm("CLC", &[0x18]);
    assert_asm("CLD", &[0xd8]);
    assert_asm("CLI", &[0x58]);
    assert_asm("CLV", &[0xb8]);
    assert_asm("SEC", &[0x38]);
    assert_asm("SED", &[0xf8]);
    assert_asm("SEI", &[0x78]);
}

#[test]
fn assemble_inc_instructions() {
    assert_asm("INC !$1234", &[0xee, 0x34, 0x12]);
    assert_asm("INC !$1234, X", &[0xfe, 0x34, 0x12]);
    assert_asm("INC $12", &[0xe6, 0x12]);
    assert_asm("INC $12, X", &[0xf6, 0x12]);
    assert_asm("INX", &[0xe8]);
    assert_asm("INY", &[0xc8]);
}

#[test]
fn assemble_jump_instructions() {
    assert_asm("JMP !$1234", &[0x4c, 0x34, 0x12]);
    assert_asm("JMP (!$1234)", &[0x6c, 0x34, 0x12]);
    assert_asm("JSR !$1234", &[0x20, 0x34, 0x12]);
}

#[test]
fn assemble_lda_instructions() {
    assert_asm("LDA #$12", &[0xa9, 0x12]);
    assert_asm("LDA !$1234", &[0xad, 0x34, 0x12]);
    assert_asm("LDA !$1234, X", &[0xbd, 0x34, 0x12]);
    assert_asm("LDA !$1234, Y", &[0xb9, 0x34, 0x12]);
    assert_asm("LDA $12", &[0xa5, 0x12]);
    assert_asm("LDA $12, X", &[0xb5, 0x12]);
    assert_asm("LDA ($12, X)", &[0xa1, 0x12]);
    assert_asm("LDA ($12), Y", &[0xb1, 0x12]);
}

#[test]
fn assemble_ldx_instructions() {
    assert_asm("LDX #$12", &[0xa2, 0x12]);
    assert_asm("LDX !$1234", &[0xae, 0x34, 0x12]);
    assert_asm("LDX !$1234, Y", &[0xbe, 0x34, 0x12]);
    assert_asm("LDX $12", &[0xa6, 0x12]);
    assert_asm("LDX $12, Y", &[0xb6, 0x12]);
}

#[test]
fn assemble_ldy_instructions() {
    assert_asm("LDY #$12", &[0xa0, 0x12]);
    assert_asm("LDY !$1234", &[0xac, 0x34, 0x12]);
    assert_asm("LDY !$1234, X", &[0xbc, 0x34, 0x12]);
    assert_asm("LDY $12", &[0xa4, 0x12]);
    assert_asm("LDY $12, X", &[0xb4, 0x12]);
}

#[test]
fn assemble_lsr_instructions() {
    assert_asm("LSR A", &[0x4a]);
    assert_asm("LSR !$1234", &[0x4e, 0x34, 0x12]);
    assert_asm("LSR !$1234, X", &[0x5e, 0x34, 0x12]);
    assert_asm("LSR $12", &[0x46, 0x12]);
    assert_asm("LSR $12, X", &[0x56, 0x12]);
}

#[test]
fn assemble_misc_instructions() {
    assert_asm("BRK #$12", &[0x00, 0x12]);
    assert_asm("NOP", &[0xea]);
    assert_asm("RTI", &[0x40]);
    assert_asm("RTS", &[0x60]);
}

#[test]
fn assemble_ora_instructions() {
    assert_asm("ORA #$12", &[0x09, 0x12]);
    assert_asm("ORA !$1234", &[0x0d, 0x34, 0x12]);
    assert_asm("ORA !$1234, X", &[0x1d, 0x34, 0x12]);
    assert_asm("ORA !$1234, Y", &[0x19, 0x34, 0x12]);
    assert_asm("ORA $12", &[0x05, 0x12]);
    assert_asm("ORA $12, X", &[0x15, 0x12]);
    assert_asm("ORA ($12, X)", &[0x01, 0x12]);
    assert_asm("ORA ($12), Y", &[0x11, 0x12]);
}

#[test]
fn assemble_push_pull_instructions() {
    assert_asm("PHA", &[0x48]);
    assert_asm("PHP", &[0x08]);
    assert_asm("PLA", &[0x68]);
    assert_asm("PLP", &[0x28]);
}

#[test]
fn assemble_rol_instructions() {
    assert_asm("ROL A", &[0x2a]);
    assert_asm("ROL !$1234", &[0x2e, 0x34, 0x12]);
    assert_asm("ROL !$1234, X", &[0x3e, 0x34, 0x12]);
    assert_asm("ROL $12", &[0x26, 0x12]);
    assert_asm("ROL $12, X", &[0x36, 0x12]);
}

#[test]
fn assemble_ror_instructions() {
    assert_asm("ROR A", &[0x6a]);
    assert_asm("ROR !$1234", &[0x6e, 0x34, 0x12]);
    assert_asm("ROR !$1234, X", &[0x7e, 0x34, 0x12]);
    assert_asm("ROR $12", &[0x66, 0x12]);
    assert_asm("ROR $12, X", &[0x76, 0x12]);
}

#[test]
fn assemble_sbc_instructions() {
    assert_asm("SBC #$12", &[0xe9, 0x12]);
    assert_asm("SBC !$1234", &[0xed, 0x34, 0x12]);
    assert_asm("SBC !$1234, X", &[0xfd, 0x34, 0x12]);
    assert_asm("SBC !$1234, Y", &[0xf9, 0x34, 0x12]);
    assert_asm("SBC $12", &[0xe5, 0x12]);
    assert_asm("SBC $12, X", &[0xf5, 0x12]);
    assert_asm("SBC ($12, X)", &[0xe1, 0x12]);
    assert_asm("SBC ($12), Y", &[0xf1, 0x12]);
}

#[test]
fn assemble_sta_instructions() {
    assert_asm("STA !$1234", &[0x8d, 0x34, 0x12]);
    assert_asm("STA !$1234, X", &[0x9d, 0x34, 0x12]);
    assert_asm("STA !$1234, Y", &[0x99, 0x34, 0x12]);
    assert_asm("STA $12", &[0x85, 0x12]);
    assert_asm("STA $12, X", &[0x95, 0x12]);
    assert_asm("STA ($12, X)", &[0x81, 0x12]);
    assert_asm("STA ($12), Y", &[0x91, 0x12]);
}

#[test]
fn assemble_stx_instructions() {
    assert_asm("STX !$1234", &[0x8e, 0x34, 0x12]);
    assert_asm("STX $12", &[0x86, 0x12]);
    assert_asm("STX $12, Y", &[0x96, 0x12]);
}

#[test]
fn assemble_sty_instructions() {
    assert_asm("STY !$1234", &[0x8c, 0x34, 0x12]);
    assert_asm("STY $12", &[0x84, 0x12]);
    assert_asm("STY $12, X", &[0x94, 0x12]);
}

#[test]
fn assemble_transfer_instructions() {
    assert_asm("TAX", &[0xaa]);
    assert_asm("TAY", &[0xa8]);
    assert_asm("TSX", &[0xba]);
    assert_asm("TXA", &[0x8a]);
    assert_asm("TXS", &[0x9a]);
    assert_asm("TYA", &[0x98]);
}

#[test]
fn assemble_undocumented_instructions() {
    assert_asm("jam", &[0x02]);
}

//===========================================================================//
