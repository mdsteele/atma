use atma::addr::Addr;
use atma::bus::{SimBus, new_ram_bus};
use atma::proc::{Mos6502, SimProc};

//===========================================================================//

fn make_test_bus(addr_bits: u32, data: &[u8]) -> Box<dyn SimBus> {
    let mut ram = vec![0u8; 1 << addr_bits];
    ram[..data.len()].copy_from_slice(data);
    new_ram_bus(ram.into_boxed_slice())
}

fn test_opcode_with_bus(
    bus: &mut dyn SimBus,
    assembly: &str,
    registers_before: [u8; 5],
    registers_after: [u8; 5],
    pc_after: u16,
) {
    let mut proc = Mos6502::new();
    proc.set_pc(Addr::from(0x0000u16));
    let (_, disassembly) = proc.disassemble(bus, proc.pc());
    assert_eq!(disassembly, assembly);
    proc.set_register("A", u32::from(registers_before[0]));
    proc.set_register("X", u32::from(registers_before[1]));
    proc.set_register("Y", u32::from(registers_before[2]));
    proc.set_register("S", u32::from(registers_before[3]));
    proc.set_register("P", u32::from(registers_before[4]));
    assert_eq!(proc.step(bus), Ok(()));
    assert_eq!(proc.pc(), Addr::from(pc_after));
    assert_eq!(proc.get_register("A"), Some(u32::from(registers_after[0])));
    assert_eq!(proc.get_register("X"), Some(u32::from(registers_after[1])));
    assert_eq!(proc.get_register("Y"), Some(u32::from(registers_after[2])));
    assert_eq!(proc.get_register("S"), Some(u32::from(registers_after[3])));
    assert_eq!(proc.get_register("P"), Some(u32::from(registers_after[4])));
}

fn test_opcode_with_data(
    code: &[u8],
    assembly: &str,
    registers_before: [u8; 5],
    registers_after: [u8; 5],
    pc_after: u16,
) {
    test_opcode_with_bus(
        &mut *make_test_bus(4, code),
        assembly,
        registers_before,
        registers_after,
        pc_after,
    );
}

#[test]
fn op_asl_accumulator() {
    test_opcode_with_data(
        &[0x0a],
        "ASL A",
        [0x9b, 0x34, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        [0x36, 0x34, 0x56, 0x78, 0x01], // A, X, Y, S, P=C
        0x0001,
    );
    test_opcode_with_data(
        &[0x0a],
        "ASL A",
        [0x36, 0x34, 0x56, 0x78, 0x01], // A, X, Y, S, P=C
        [0x6c, 0x34, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        0x0001,
    );
}

#[test]
fn op_asl_zero_page() {
    let mut bus = make_test_bus(8, &[0x06, 0x02, 0x89]);
    test_opcode_with_bus(
        &mut *bus,
        "ASL $02",
        [0x12, 0x34, 0x56, 0x78, 0x02], // A, X, Y, S, P=Z
        [0x12, 0x34, 0x56, 0x78, 0x01], // A, X, Y, S, P=C
        0x0002,
    );
    assert_eq!(bus.peek_byte(Addr::from(0x02u8)), 0x12);
}

#[test]
fn op_bit_absolute() {
    test_opcode_with_data(
        &[0x2c, 0x03, 0x00, 0x41],
        "BIT $0003",
        [0x06, 0x34, 0x56, 0x78, 0x88], // A, X, Y, S, P=ND
        [0x06, 0x34, 0x56, 0x78, 0x4a], // A, X, Y, S, P=VDZ
        0x0003,
    );
}

#[test]
fn op_brk() {
    let mut bus = make_test_bus(8, &[0x00, 0xb2]);
    bus.write_byte(Addr::from(0xfffeu16), 0x34);
    bus.write_byte(Addr::from(0xffffu16), 0x12);
    test_opcode_with_bus(
        &mut *bus,
        "BRK #$b2",
        [0x12, 0x34, 0x56, 0x78, 0x08], // A, X, Y, S, P=D
        [0x12, 0x34, 0x56, 0x75, 0x0c], // A, X, Y, S, P=DI
        0x1234,
    );
    assert_eq!(bus.peek_byte(Addr::from(0x0176u16)), 0x38); // saved P register
    assert_eq!(bus.peek_byte(Addr::from(0x0177u16)), 0x02); // return addr lo
    assert_eq!(bus.peek_byte(Addr::from(0x0178u16)), 0x00); // return addr hi
}

#[test]
fn op_cld() {
    test_opcode_with_data(
        &[0xd8],
        "CLD",
        [0x12, 0x34, 0x56, 0x78, 0x88], // A, X, Y, S, P=ND
        [0x12, 0x34, 0x56, 0x78, 0x80], // A, X, Y, S, P=N
        0x0001,
    );
}

#[test]
fn op_cmp_immediate() {
    test_opcode_with_data(
        &[0xc9, 0x10],
        "CMP #$10",
        [0x10, 0x34, 0x56, 0x78, 0x80], // A, X, Y, S, P=N
        [0x10, 0x34, 0x56, 0x78, 0x03], // A, X, Y, S, P=ZC
        0x0002,
    );
    test_opcode_with_data(
        &[0xc9, 0x10],
        "CMP #$10",
        [0x0f, 0x34, 0x56, 0x78, 0x03], // A, X, Y, S, P=ZC
        [0x0f, 0x34, 0x56, 0x78, 0x80], // A, X, Y, S, P=N
        0x0002,
    );
}

#[test]
fn op_dec_zero_page() {
    let mut bus = make_test_bus(4, &[0xc6, 0x02, 0x01]);
    test_opcode_with_bus(
        &mut *bus,
        "DEC $02",
        [0x12, 0x34, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        [0x12, 0x34, 0x56, 0x78, 0x02], // A, X, Y, S, P=Z
        0x0002,
    );
    assert_eq!(bus.peek_byte(Addr::from(0x02u8)), 0x00);
    test_opcode_with_bus(
        &mut *bus,
        "DEC $02",
        [0x12, 0x34, 0x56, 0x78, 0x02], // A, X, Y, S, P=Z
        [0x12, 0x34, 0x56, 0x78, 0x80], // A, X, Y, S, P=N
        0x0002,
    );
    assert_eq!(bus.peek_byte(Addr::from(0x02u8)), 0xff);
}

#[test]
fn op_dex() {
    test_opcode_with_data(
        &[0xca],
        "DEX",
        [0x12, 0x01, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        [0x12, 0x00, 0x56, 0x78, 0x02], // A, X, Y, S, P=Z
        0x0001,
    );
    test_opcode_with_data(
        &[0xca],
        "DEX",
        [0x12, 0x00, 0x56, 0x78, 0x02], // A, X, Y, S, P=Z
        [0x12, 0xff, 0x56, 0x78, 0x80], // A, X, Y, S, P=N
        0x0001,
    );
}

#[test]
fn op_lda_immediate() {
    test_opcode_with_data(
        &[0xa9, 0x00],
        "LDA #$00",
        [0xff, 0x00, 0x00, 0x00, 0x00], // A, X, Y, S, P=0
        [0x00, 0x00, 0x00, 0x00, 0x02], // A, X, Y, S, P=Z
        0x0002,
    );
}

#[test]
fn op_lda_absolute() {
    test_opcode_with_data(
        &[0xad, 0x03, 0x00, 0x01],
        "LDA $0003",
        [0x00, 0x00, 0x00, 0x00, 0x00], // A, X, Y, S, P=0
        [0x01, 0x00, 0x00, 0x00, 0x00], // A, X, Y, S, P=0
        0x0003,
    );
}

#[test]
fn op_lda_zero_page() {
    test_opcode_with_data(
        &[0xa5, 0x02, 0x80],
        "LDA $02",
        [0x00, 0x00, 0x00, 0x00, 0x00], // A, X, Y, S, P=0
        [0x80, 0x00, 0x00, 0x00, 0x80], // A, X, Y, S, P=N
        0x0002,
    );
}

#[test]
fn op_lda_zero_page_indirect_y_indexed() {
    test_opcode_with_data(
        &[0xb1, 0x02, 0x04, 0x00, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4],
        "LDA ($02), Y",
        [0x00, 0x00, 0x03, 0x00, 0x00], // A, X, Y, S, P=0
        [0xd3, 0x00, 0x03, 0x00, 0x80], // A, X, Y, S, P=N
        0x0002,
    );
}

#[test]
fn op_nop() {
    test_opcode_with_data(
        &[0xea],
        "NOP",
        [0x12, 0x34, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        [0x12, 0x34, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        0x0001,
    );
}

#[test]
fn op_rol_accumulator() {
    test_opcode_with_data(
        &[0x2a],
        "ROL A",
        [0x9b, 0x34, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        [0x36, 0x34, 0x56, 0x78, 0x01], // A, X, Y, S, P=C
        0x0001,
    );
    test_opcode_with_data(
        &[0x2a],
        "ROL A",
        [0x36, 0x34, 0x56, 0x78, 0x01], // A, X, Y, S, P=C
        [0x6d, 0x34, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        0x0001,
    );
}

#[test]
fn op_sed() {
    test_opcode_with_data(
        &[0xf8],
        "SED",
        [0x12, 0x34, 0x56, 0x78, 0x00], // A, X, Y, S, P=0
        [0x12, 0x34, 0x56, 0x78, 0x08], // A, X, Y, S, P=D
        0x0001,
    );
}

#[test]
fn op_sta_zero_page() {
    let mut bus = make_test_bus(4, &[0x85, 0x0e]);
    test_opcode_with_bus(
        &mut *bus,
        "STA $0e",
        [0xab, 0x00, 0x00, 0x00, 0x00], // A, X, Y, S, P=0
        [0xab, 0x00, 0x00, 0x00, 0x00], // A, X, Y, S, P=0
        0x0002,
    );
    assert_eq!(bus.peek_byte(Addr::from(0x0eu8)), 0xab);
}

#[test]
fn op_sta_x_indexed_zero_page_indirect() {
    let mut bus = make_test_bus(9, &[0x81, 0x02, 0x00, 0x00, 0x23, 0x01]);
    test_opcode_with_bus(
        &mut *bus,
        "STA ($02, X)",
        [0xcd, 0x02, 0x00, 0x00, 0x00], // A, X, Y, S, P=0
        [0xcd, 0x02, 0x00, 0x00, 0x00], // A, X, Y, S, P=0
        0x0002,
    );
    assert_eq!(bus.peek_byte(Addr::from(0x0123u16)), 0xcd);
}

//===========================================================================//
