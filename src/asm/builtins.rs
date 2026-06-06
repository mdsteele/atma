use super::arch::ArchTree;
use super::macros::MacroTable;
use crate::addr::Endianness;
use crate::error::SrcSpan;
use crate::parse::{
    AsmAssertAst, AsmDefMacroAst, AsmIntDataAst, AsmIntTypeAst,
    AsmMacroArgAst, AsmStmtAst, BinOpAst, ExprAst, ExprAstNode, IdentifierAst,
    Token, TokenValue,
};
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

const MACROS_65XX: &[(&str, u8, AddrMode)] = &[
    ("ADC", 0x65, AddrMode::Addr8),
    ("ADC", 0x75, AddrMode::Addr8CommaReg(Reg::X)),
    ("ADC", 0x61, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("ADC", 0x69, AddrMode::PoundImm8),
    ("ADC", 0x6d, AddrMode::BangAddr16),
    ("ADC", 0x71, AddrMode::ParAddr8EnsCommaReg(Reg::Y)),
    ("ADC", 0x79, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("ADC", 0x7d, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("AND", 0x25, AddrMode::Addr8),
    ("AND", 0x35, AddrMode::Addr8CommaReg(Reg::X)),
    ("AND", 0x21, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("AND", 0x29, AddrMode::PoundImm8),
    ("AND", 0x2d, AddrMode::BangAddr16),
    ("AND", 0x31, AddrMode::ParAddr8EnsCommaReg(Reg::Y)),
    ("AND", 0x39, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("AND", 0x3d, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("ASL", 0x06, AddrMode::Addr8),
    ("ASL", 0x0e, AddrMode::BangAddr16),
    ("ASL", 0x0a, AddrMode::Reg(Reg::A)),
    ("ASL", 0x16, AddrMode::Addr8CommaReg(Reg::X)),
    ("ASL", 0x1e, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("BCC", 0x90, AddrMode::Branch),
    ("BCS", 0xb0, AddrMode::Branch),
    ("BEQ", 0xf0, AddrMode::Branch),
    ("BIT", 0x24, AddrMode::Addr8),
    ("BIT", 0x2c, AddrMode::BangAddr16),
    ("BMI", 0x30, AddrMode::Branch),
    ("BNE", 0xd0, AddrMode::Branch),
    ("BPL", 0x10, AddrMode::Branch),
    ("BRK", 0x00, AddrMode::PoundImm8),
    ("BVC", 0x50, AddrMode::Branch),
    ("BVS", 0x70, AddrMode::Branch),
    ("CLC", 0x18, AddrMode::Implied),
    ("CLD", 0xd8, AddrMode::Implied),
    ("CLI", 0x58, AddrMode::Implied),
    ("CLV", 0xb8, AddrMode::Implied),
    ("CMP", 0xc5, AddrMode::Addr8),
    ("CMP", 0xd5, AddrMode::Addr8CommaReg(Reg::X)),
    ("CMP", 0xc1, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("CMP", 0xc9, AddrMode::PoundImm8),
    ("CMP", 0xcd, AddrMode::BangAddr16),
    ("CMP", 0xd1, AddrMode::ParAddr8EnsCommaReg(Reg::Y)),
    ("CMP", 0xd9, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("CMP", 0xdd, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("CPX", 0xe4, AddrMode::Addr8),
    ("CPX", 0xec, AddrMode::BangAddr16),
    ("CPX", 0xe0, AddrMode::PoundImm8),
    ("CPY", 0xc4, AddrMode::Addr8),
    ("CPY", 0xcc, AddrMode::BangAddr16),
    ("CPY", 0xc0, AddrMode::PoundImm8),
    ("DEC", 0xc6, AddrMode::Addr8),
    ("DEC", 0xce, AddrMode::BangAddr16),
    ("DEC", 0xd6, AddrMode::Addr8CommaReg(Reg::X)),
    ("DEC", 0xde, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("DEX", 0xca, AddrMode::Implied),
    ("DEY", 0x88, AddrMode::Implied),
    ("EOR", 0x45, AddrMode::Addr8),
    ("EOR", 0x55, AddrMode::Addr8CommaReg(Reg::X)),
    ("EOR", 0x41, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("EOR", 0x49, AddrMode::PoundImm8),
    ("EOR", 0x4d, AddrMode::BangAddr16),
    ("EOR", 0x51, AddrMode::ParAddr8EnsCommaReg(Reg::Y)),
    ("EOR", 0x59, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("EOR", 0x5d, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("INC", 0xe6, AddrMode::Addr8),
    ("INC", 0xee, AddrMode::BangAddr16),
    ("INC", 0xf6, AddrMode::Addr8CommaReg(Reg::X)),
    ("INC", 0xfe, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("INX", 0xe8, AddrMode::Implied),
    ("INY", 0xc8, AddrMode::Implied),
    ("JMP", 0x4c, AddrMode::BangAddr16),
    ("JMP", 0x6c, AddrMode::ParBangAddr16Ens),
    ("JSR", 0x20, AddrMode::BangAddr16),
    ("LDA", 0xa5, AddrMode::Addr8),
    ("LDA", 0xa1, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("LDA", 0xa9, AddrMode::PoundImm8),
    ("LDA", 0xad, AddrMode::BangAddr16),
    ("LDA", 0xb1, AddrMode::ParAddr8EnsCommaReg(Reg::Y)),
    ("LDA", 0xb5, AddrMode::Addr8CommaReg(Reg::X)),
    ("LDA", 0xb9, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("LDA", 0xbd, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("LDX", 0xa6, AddrMode::Addr8),
    ("LDX", 0xa2, AddrMode::PoundImm8),
    ("LDX", 0xae, AddrMode::BangAddr16),
    ("LDX", 0xb6, AddrMode::Addr8CommaReg(Reg::Y)),
    ("LDX", 0xbe, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("LDY", 0xa4, AddrMode::Addr8),
    ("LDY", 0xa0, AddrMode::PoundImm8),
    ("LDY", 0xac, AddrMode::BangAddr16),
    ("LDY", 0xb4, AddrMode::Addr8CommaReg(Reg::X)),
    ("LDY", 0xbc, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("LSR", 0x46, AddrMode::Addr8),
    ("LSR", 0x4e, AddrMode::BangAddr16),
    ("LSR", 0x4a, AddrMode::Reg(Reg::A)),
    ("LSR", 0x56, AddrMode::Addr8CommaReg(Reg::X)),
    ("LSR", 0x5e, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("NOP", 0xea, AddrMode::Implied),
    ("ORA", 0x05, AddrMode::Addr8),
    ("ORA", 0x15, AddrMode::Addr8CommaReg(Reg::X)),
    ("ORA", 0x01, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("ORA", 0x09, AddrMode::PoundImm8),
    ("ORA", 0x0d, AddrMode::BangAddr16),
    ("ORA", 0x11, AddrMode::ParAddr8EnsCommaReg(Reg::Y)),
    ("ORA", 0x19, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("ORA", 0x1d, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("PHA", 0x48, AddrMode::Implied),
    ("PHP", 0x08, AddrMode::Implied),
    ("PLA", 0x68, AddrMode::Implied),
    ("PLP", 0x28, AddrMode::Implied),
    ("ROL", 0x26, AddrMode::Addr8),
    ("ROL", 0x2a, AddrMode::Reg(Reg::A)),
    ("ROL", 0x2e, AddrMode::BangAddr16),
    ("ROL", 0x36, AddrMode::Addr8CommaReg(Reg::X)),
    ("ROL", 0x3e, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("ROR", 0x66, AddrMode::Addr8),
    ("ROR", 0x6a, AddrMode::Reg(Reg::A)),
    ("ROR", 0x6e, AddrMode::BangAddr16),
    ("ROR", 0x76, AddrMode::Addr8CommaReg(Reg::X)),
    ("ROR", 0x7e, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("RTI", 0x40, AddrMode::Implied),
    ("RTS", 0x60, AddrMode::Implied),
    ("SBC", 0xe5, AddrMode::Addr8),
    ("SBC", 0xf5, AddrMode::Addr8CommaReg(Reg::X)),
    ("SBC", 0xe1, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("SBC", 0xe9, AddrMode::PoundImm8),
    ("SBC", 0xed, AddrMode::BangAddr16),
    ("SBC", 0xf1, AddrMode::ParAddr8EnsCommaReg(Reg::Y)),
    ("SBC", 0xf9, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("SBC", 0xfd, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("SEC", 0x38, AddrMode::Implied),
    ("SED", 0xf8, AddrMode::Implied),
    ("SEI", 0x78, AddrMode::Implied),
    ("STA", 0x85, AddrMode::Addr8),
    ("STA", 0x81, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("STA", 0x8d, AddrMode::BangAddr16),
    ("STA", 0x91, AddrMode::ParAddr8EnsCommaReg(Reg::Y)),
    ("STA", 0x95, AddrMode::Addr8CommaReg(Reg::X)),
    ("STA", 0x99, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("STA", 0x9d, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("STX", 0x86, AddrMode::Addr8),
    ("STX", 0x8e, AddrMode::BangAddr16),
    ("STX", 0x96, AddrMode::Addr8CommaReg(Reg::Y)),
    ("STY", 0x84, AddrMode::Addr8),
    ("STY", 0x8c, AddrMode::BangAddr16),
    ("STY", 0x94, AddrMode::Addr8CommaReg(Reg::X)),
    ("TAX", 0xaa, AddrMode::Implied),
    ("TAY", 0xa8, AddrMode::Implied),
    ("TSX", 0xba, AddrMode::Implied),
    ("TXA", 0x8a, AddrMode::Implied),
    ("TXS", 0x9a, AddrMode::Implied),
    ("TYA", 0x98, AddrMode::Implied),
];

const MACROS_6502: &[(&str, u8, AddrMode)] =
    &[("JAM", 0x02, AddrMode::Implied)];

const MACROS_65C816: &[(&str, u8, AddrMode)] = &[
    ("ADC", 0x69, AddrMode::PoundPoundImm16),
    ("BIT", 0x34, AddrMode::Addr8CommaReg(Reg::X)),
    ("BIT", 0x3c, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("BIT", 0x89, AddrMode::PoundImm8),
    ("BIT", 0x89, AddrMode::PoundPoundImm16),
    ("BRA", 0x80, AddrMode::Branch),
    ("COP", 0x02, AddrMode::PoundImm8),
    ("DEC", 0x3a, AddrMode::Reg(Reg::A)),
    ("INC", 0x1a, AddrMode::Reg(Reg::A)),
    ("JML", 0x5c, AddrMode::BangBangAddr24),
    ("JML", 0xdc, AddrMode::BracBangAddr16Kets),
    ("JMP", 0x7c, AddrMode::ParBangAddr16CommaRegEns(Reg::X)),
    ("JSL", 0x22, AddrMode::BangBangAddr24),
    ("JSR", 0xfc, AddrMode::ParBangAddr16CommaRegEns(Reg::X)),
    ("LDA", 0xa9, AddrMode::PoundPoundImm16),
    ("LDA", 0xb2, AddrMode::ParAddr8Ens),
    ("LDA", 0xa7, AddrMode::BracAddr8Kets),
    ("LDA", 0xb7, AddrMode::BracAddr8KetsCommaReg(Reg::Y)),
    ("LDA", 0xaf, AddrMode::BangBangAddr24),
    ("LDA", 0xbf, AddrMode::BangBangAddr24CommaReg(Reg::X)),
    ("LDA", 0xa3, AddrMode::Addr8CommaReg(Reg::S)),
    ("LDA", 0xb3, AddrMode::ParAddr8CommaRegEnsCommaReg(Reg::S, Reg::Y)),
    ("LDX", 0xa2, AddrMode::PoundPoundImm16),
    ("LDY", 0xa0, AddrMode::PoundPoundImm16),
    ("MVN", 0x54, AddrMode::PoundImm8CommaPoundImm8),
    ("MVP", 0x44, AddrMode::PoundImm8CommaPoundImm8),
    ("PEA", 0xf4, AddrMode::BangAddr16),
    ("PEI", 0xd4, AddrMode::ParAddr8Ens),
    ("PHB", 0x8b, AddrMode::Implied),
    ("PHD", 0x0b, AddrMode::Implied),
    ("PHK", 0x4b, AddrMode::Implied),
    ("PHX", 0xda, AddrMode::Implied),
    ("PHY", 0x5a, AddrMode::Implied),
    ("PLB", 0xab, AddrMode::Implied),
    ("PLD", 0x2b, AddrMode::Implied),
    ("PLX", 0xfa, AddrMode::Implied),
    ("PLY", 0x7a, AddrMode::Implied),
    ("REP", 0xc2, AddrMode::PoundImm8),
    ("RTL", 0x6b, AddrMode::Implied),
    ("SEP", 0xe2, AddrMode::PoundImm8),
    ("STA", 0x92, AddrMode::ParAddr8Ens),
    ("STA", 0x87, AddrMode::BracAddr8Kets),
    ("STA", 0x97, AddrMode::BracAddr8KetsCommaReg(Reg::Y)),
    ("STA", 0x8f, AddrMode::BangBangAddr24),
    ("STA", 0x9f, AddrMode::BangBangAddr24CommaReg(Reg::X)),
    ("STA", 0x83, AddrMode::Addr8CommaReg(Reg::S)),
    ("STA", 0x93, AddrMode::ParAddr8CommaRegEnsCommaReg(Reg::S, Reg::Y)),
    ("STP", 0xdb, AddrMode::Implied),
    ("STZ", 0x64, AddrMode::Addr8),
    ("STZ", 0x74, AddrMode::Addr8CommaReg(Reg::X)),
    ("STZ", 0x9c, AddrMode::BangAddr16),
    ("STZ", 0x9e, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("TCD", 0x5b, AddrMode::Implied),
    ("TCS", 0x1b, AddrMode::Implied),
    ("TDC", 0x7b, AddrMode::Implied),
    ("TSC", 0x3b, AddrMode::Implied),
    ("TXY", 0x9b, AddrMode::Implied),
    ("TYX", 0xbb, AddrMode::Implied),
    ("WAI", 0xcb, AddrMode::Implied),
    ("WDM", 0x42, AddrMode::PoundImm8),
    ("XBA", 0xeb, AddrMode::Implied),
    ("XCE", 0xfb, AddrMode::Implied),
    // TODO: Remove these redundances with 65xx macros, once the tests can pass
    // without them:
    ("LDA", 0xa1, AddrMode::ParAddr8CommaRegEns(Reg::X)),
    ("STA", 0x81, AddrMode::ParAddr8CommaRegEns(Reg::X)),
];

const MACROS_SM83: &[(&str, u8, AddrMode)] = &[
    ("CALL", 0xcd, AddrMode::Addr16),
    ("CCF", 0x3d, AddrMode::Implied),
    ("CPL", 0x2f, AddrMode::Implied),
    ("DAA", 0x27, AddrMode::Implied),
    ("DEC", 0x05, AddrMode::Reg(Reg::B)),
    ("DEC", 0x0d, AddrMode::Reg(Reg::C)),
    ("DEC", 0x15, AddrMode::Reg(Reg::D)),
    ("DEC", 0x1d, AddrMode::Reg(Reg::E)),
    ("DEC", 0x25, AddrMode::Reg(Reg::H)),
    ("DEC", 0x2d, AddrMode::Reg(Reg::L)),
    ("DEC", 0x3d, AddrMode::Reg(Reg::A)),
    ("DI", 0xf3, AddrMode::Implied),
    ("EI", 0xfb, AddrMode::Implied),
    ("HALT", 0x76, AddrMode::Implied),
    ("JP", 0xc3, AddrMode::Addr16),
    ("JR", 0x18, AddrMode::Branch),
    ("LD", 0x40, AddrMode::RegCommaReg(Reg::B, Reg::B)),
    ("LD", 0x41, AddrMode::RegCommaReg(Reg::B, Reg::C)),
    ("LD", 0x42, AddrMode::RegCommaReg(Reg::B, Reg::D)),
    ("LD", 0x43, AddrMode::RegCommaReg(Reg::B, Reg::E)),
    ("LD", 0x44, AddrMode::RegCommaReg(Reg::B, Reg::H)),
    ("LD", 0x45, AddrMode::RegCommaReg(Reg::B, Reg::L)),
    ("LD", 0x47, AddrMode::RegCommaReg(Reg::B, Reg::A)),
    ("LD", 0x48, AddrMode::RegCommaReg(Reg::C, Reg::B)),
    ("LD", 0x49, AddrMode::RegCommaReg(Reg::C, Reg::C)),
    ("LD", 0x4a, AddrMode::RegCommaReg(Reg::C, Reg::D)),
    ("LD", 0x4b, AddrMode::RegCommaReg(Reg::C, Reg::E)),
    ("LD", 0x4c, AddrMode::RegCommaReg(Reg::C, Reg::H)),
    ("LD", 0x4d, AddrMode::RegCommaReg(Reg::C, Reg::L)),
    ("LD", 0x4f, AddrMode::RegCommaReg(Reg::C, Reg::A)),
    ("NOP", 0x00, AddrMode::Implied),
    ("RET", 0xc9, AddrMode::Implied),
    ("RETI", 0xd9, AddrMode::Implied),
    ("RLA", 0x17, AddrMode::Implied),
    ("RLCA", 0x07, AddrMode::Implied),
    ("RRA", 0x1f, AddrMode::Implied),
    ("RRCA", 0x0f, AddrMode::Implied),
    ("SCF", 0x37, AddrMode::Implied),
    ("STOP", 0x10, AddrMode::Implied),
];

const MACROS_SPC700: &[(&str, u8, AddrMode)] = &[
    ("ADC", 0x84, AddrMode::RegCommaAddr8(Reg::A)),
    ("ADC", 0x85, AddrMode::RegCommaBangAddr16(Reg::A)),
    ("ADC", 0x86, AddrMode::RegCommaParRegEns(Reg::A, Reg::X)),
    ("ADC", 0x87, AddrMode::RegCommaBracAddr8PlusRegKets(Reg::A, Reg::X)),
    ("ADC", 0x88, AddrMode::RegCommaPoundImm8(Reg::A)),
    ("ADC", 0x89, AddrMode::Addr8CommaAddr8),
    ("ADC", 0x94, AddrMode::RegCommaAddr8PlusReg(Reg::A, Reg::X)),
    ("ADC", 0x95, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::X)),
    ("ADC", 0x96, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::Y)),
    ("ADC", 0x97, AddrMode::RegCommaBracAddr8KetsPlusReg(Reg::A, Reg::Y)),
    ("ADC", 0x98, AddrMode::Addr8CommaPoundImm8),
    ("ADC", 0x99, AddrMode::ParRegEnsCommaParRegEns(Reg::X, Reg::Y)),
    ("ADDW", 0x7a, AddrMode::RegCommaAddr8(Reg::Ya)),
    ("AND", 0x24, AddrMode::RegCommaAddr8(Reg::A)),
    ("AND", 0x25, AddrMode::RegCommaBangAddr16(Reg::A)),
    ("AND", 0x26, AddrMode::RegCommaParRegEns(Reg::A, Reg::X)),
    ("AND", 0x27, AddrMode::RegCommaBracAddr8PlusRegKets(Reg::A, Reg::X)),
    ("AND", 0x28, AddrMode::RegCommaPoundImm8(Reg::A)),
    ("AND", 0x29, AddrMode::Addr8CommaAddr8),
    ("AND", 0x34, AddrMode::RegCommaAddr8PlusReg(Reg::A, Reg::X)),
    ("AND", 0x35, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::X)),
    ("AND", 0x36, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::Y)),
    ("AND", 0x37, AddrMode::RegCommaBracAddr8KetsPlusReg(Reg::A, Reg::Y)),
    ("AND", 0x38, AddrMode::Addr8CommaPoundImm8),
    ("AND", 0x39, AddrMode::ParRegEnsCommaParRegEns(Reg::X, Reg::Y)),
    ("ASL", 0x0b, AddrMode::Addr8),
    ("ASL", 0x0c, AddrMode::BangAddr16),
    ("ASL", 0x1b, AddrMode::Addr8PlusReg(Reg::X)),
    ("ASL", 0x1c, AddrMode::Reg(Reg::A)),
    ("BCC", 0x90, AddrMode::Branch),
    ("BCS", 0xb0, AddrMode::Branch),
    ("BEQ", 0xf0, AddrMode::Branch),
    ("BMI", 0x30, AddrMode::Branch),
    ("BNE", 0xd0, AddrMode::Branch),
    ("BPL", 0x10, AddrMode::Branch),
    ("BRA", 0x2f, AddrMode::Branch),
    ("BRK", 0x0f, AddrMode::Implied),
    ("BVC", 0x50, AddrMode::Branch),
    ("BVS", 0x70, AddrMode::Branch),
    ("CALL", 0x3f, AddrMode::BangAddr16),
    ("CLRC", 0x60, AddrMode::Implied),
    ("CLRP", 0x20, AddrMode::Implied),
    ("CLRV", 0xe0, AddrMode::Implied),
    ("CMP", 0x1e, AddrMode::RegCommaBangAddr16(Reg::X)),
    ("CMP", 0x3e, AddrMode::RegCommaAddr8(Reg::X)),
    ("CMP", 0x5e, AddrMode::RegCommaBangAddr16(Reg::Y)),
    ("CMP", 0x64, AddrMode::RegCommaAddr8(Reg::A)),
    ("CMP", 0x65, AddrMode::RegCommaBangAddr16(Reg::A)),
    ("CMP", 0x66, AddrMode::RegCommaParRegEns(Reg::A, Reg::X)),
    ("CMP", 0x67, AddrMode::RegCommaBracAddr8PlusRegKets(Reg::A, Reg::X)),
    ("CMP", 0x68, AddrMode::RegCommaPoundImm8(Reg::A)),
    ("CMP", 0x69, AddrMode::Addr8CommaAddr8),
    ("CMP", 0x74, AddrMode::RegCommaAddr8PlusReg(Reg::A, Reg::X)),
    ("CMP", 0x75, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::X)),
    ("CMP", 0x76, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::Y)),
    ("CMP", 0x77, AddrMode::RegCommaBracAddr8KetsPlusReg(Reg::A, Reg::Y)),
    ("CMP", 0x78, AddrMode::Addr8CommaPoundImm8),
    ("CMP", 0x79, AddrMode::ParRegEnsCommaParRegEns(Reg::X, Reg::Y)),
    ("CMP", 0x7e, AddrMode::RegCommaAddr8(Reg::Y)),
    ("CMP", 0xad, AddrMode::RegCommaPoundImm8(Reg::Y)),
    ("CMP", 0xc8, AddrMode::RegCommaPoundImm8(Reg::X)),
    ("CMPW", 0x5a, AddrMode::RegCommaAddr8(Reg::Ya)),
    ("DAA", 0xdf, AddrMode::Reg(Reg::A)),
    ("DAS", 0xbe, AddrMode::Reg(Reg::A)),
    ("DEC", 0x8b, AddrMode::Addr8),
    ("DEC", 0x1d, AddrMode::Reg(Reg::X)),
    ("DEC", 0x8c, AddrMode::BangAddr16),
    ("DEC", 0x8c, AddrMode::BangAddr16),
    ("DEC", 0x9b, AddrMode::Addr8PlusReg(Reg::X)),
    ("DEC", 0x9c, AddrMode::Reg(Reg::A)),
    ("DEC", 0xdc, AddrMode::Reg(Reg::Y)),
    ("DECW", 0x1a, AddrMode::Addr8),
    ("DI", 0xc0, AddrMode::Implied),
    ("DIV", 0x9e, AddrMode::RegCommaReg(Reg::Ya, Reg::X)),
    ("EI", 0xa0, AddrMode::Implied),
    ("EOR", 0x44, AddrMode::RegCommaAddr8(Reg::A)),
    ("EOR", 0x45, AddrMode::RegCommaBangAddr16(Reg::A)),
    ("EOR", 0x46, AddrMode::RegCommaParRegEns(Reg::A, Reg::X)),
    ("EOR", 0x47, AddrMode::RegCommaBracAddr8PlusRegKets(Reg::A, Reg::X)),
    ("EOR", 0x48, AddrMode::RegCommaPoundImm8(Reg::A)),
    ("EOR", 0x49, AddrMode::Addr8CommaAddr8),
    ("EOR", 0x54, AddrMode::RegCommaAddr8PlusReg(Reg::A, Reg::X)),
    ("EOR", 0x55, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::X)),
    ("EOR", 0x56, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::Y)),
    ("EOR", 0x57, AddrMode::RegCommaBracAddr8KetsPlusReg(Reg::A, Reg::Y)),
    ("EOR", 0x58, AddrMode::Addr8CommaPoundImm8),
    ("EOR", 0x59, AddrMode::ParRegEnsCommaParRegEns(Reg::X, Reg::Y)),
    ("INC", 0xab, AddrMode::Addr8),
    ("INC", 0x3d, AddrMode::Reg(Reg::X)),
    ("INC", 0xac, AddrMode::BangAddr16),
    ("INC", 0xbb, AddrMode::Addr8PlusReg(Reg::X)),
    ("INC", 0xbc, AddrMode::Reg(Reg::A)),
    ("INC", 0xfc, AddrMode::Reg(Reg::Y)),
    ("INCW", 0x3a, AddrMode::Addr8),
    ("JMP", 0x1f, AddrMode::BracBangAddr16PlusRegKets(Reg::X)),
    ("JMP", 0x5f, AddrMode::BangAddr16),
    ("LSR", 0x4b, AddrMode::Addr8),
    ("LSR", 0x4c, AddrMode::BangAddr16),
    ("LSR", 0x5b, AddrMode::Addr8PlusReg(Reg::X)),
    ("LSR", 0x5c, AddrMode::Reg(Reg::A)),
    ("MOV", 0xfa, AddrMode::Addr8CommaAddr8),
    ("MOV", 0x8f, AddrMode::Addr8CommaPoundImm8),
    ("MOV", 0xf8, AddrMode::RegCommaAddr8(Reg::X)),
    ("MOV", 0xeb, AddrMode::RegCommaAddr8(Reg::Y)),
    ("MOV", 0x8d, AddrMode::RegCommaPoundImm8(Reg::Y)),
    ("MOV", 0xcd, AddrMode::RegCommaPoundImm8(Reg::X)),
    ("MOV", 0xe4, AddrMode::RegCommaAddr8(Reg::A)),
    ("MOV", 0xe5, AddrMode::RegCommaBangAddr16(Reg::A)),
    ("MOV", 0xe6, AddrMode::RegCommaParRegEns(Reg::A, Reg::X)),
    ("MOV", 0xe7, AddrMode::RegCommaBracAddr8PlusRegKets(Reg::A, Reg::X)),
    ("MOV", 0xe8, AddrMode::RegCommaPoundImm8(Reg::A)),
    ("MOV", 0xe9, AddrMode::RegCommaBangAddr16(Reg::X)),
    ("MOV", 0xec, AddrMode::RegCommaBangAddr16(Reg::Y)),
    ("MOV", 0xf4, AddrMode::RegCommaAddr8PlusReg(Reg::A, Reg::X)),
    ("MOV", 0xf5, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::X)),
    ("MOV", 0xf6, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::Y)),
    ("MOV", 0xf7, AddrMode::RegCommaBracAddr8KetsPlusReg(Reg::A, Reg::Y)),
    ("MOV", 0xf9, AddrMode::RegCommaAddr8PlusReg(Reg::X, Reg::Y)),
    ("MOV", 0xfb, AddrMode::RegCommaAddr8PlusReg(Reg::Y, Reg::X)),
    ("MOV", 0xbf, AddrMode::RegCommaParRegEnsPlus(Reg::A, Reg::X)),
    ("MOV", 0xc4, AddrMode::Addr8CommaReg(Reg::A)),
    ("MOV", 0xd4, AddrMode::Addr8PlusRegCommaReg(Reg::X, Reg::A)),
    ("MOV", 0xc5, AddrMode::BangAddr16CommaReg(Reg::A)),
    ("MOV", 0xd5, AddrMode::BangAddr16PlusRegCommaReg(Reg::X, Reg::A)),
    ("MOV", 0xd6, AddrMode::BangAddr16PlusRegCommaReg(Reg::Y, Reg::A)),
    ("MOV", 0xc7, AddrMode::BracAddr8PlusRegKetsCommaReg(Reg::X, Reg::A)),
    ("MOV", 0xd7, AddrMode::BracAddr8KetsPlusRegCommaReg(Reg::Y, Reg::A)),
    ("MOV", 0xd8, AddrMode::Addr8CommaReg(Reg::X)),
    ("MOV", 0xd9, AddrMode::Addr8PlusRegCommaReg(Reg::Y, Reg::X)),
    ("MOV", 0xc9, AddrMode::BangAddr16CommaReg(Reg::X)),
    ("MOV", 0xcb, AddrMode::Addr8CommaReg(Reg::Y)),
    ("MOV", 0xdb, AddrMode::Addr8PlusRegCommaReg(Reg::X, Reg::Y)),
    ("MOV", 0xcc, AddrMode::BangAddr16CommaReg(Reg::Y)),
    ("MOV", 0xc6, AddrMode::ParRegEnsCommaReg(Reg::X, Reg::A)),
    ("MOV", 0xaf, AddrMode::ParRegEnsPlusCommaReg(Reg::X, Reg::A)),
    ("MOV", 0x5d, AddrMode::RegCommaReg(Reg::X, Reg::A)),
    ("MOV", 0x7d, AddrMode::RegCommaReg(Reg::A, Reg::X)),
    ("MOV", 0xfd, AddrMode::RegCommaReg(Reg::Y, Reg::A)),
    ("MOV", 0x9d, AddrMode::RegCommaReg(Reg::X, Reg::Sp)),
    ("MOV", 0xbd, AddrMode::RegCommaReg(Reg::Sp, Reg::X)),
    ("MOV", 0xdd, AddrMode::RegCommaReg(Reg::A, Reg::Y)),
    ("MOVW", 0xba, AddrMode::RegCommaAddr8(Reg::Ya)),
    ("MOVW", 0xda, AddrMode::Addr8CommaReg(Reg::Ya)),
    ("MUL", 0xcf, AddrMode::Reg(Reg::Ya)),
    ("NOP", 0x00, AddrMode::Implied),
    ("NOTC", 0xed, AddrMode::Implied),
    ("OR", 0x04, AddrMode::RegCommaAddr8(Reg::A)),
    ("OR", 0x05, AddrMode::RegCommaBangAddr16(Reg::A)),
    ("OR", 0x06, AddrMode::RegCommaParRegEns(Reg::A, Reg::X)),
    ("OR", 0x07, AddrMode::RegCommaBracAddr8PlusRegKets(Reg::A, Reg::X)),
    ("OR", 0x08, AddrMode::RegCommaPoundImm8(Reg::A)),
    ("OR", 0x09, AddrMode::Addr8CommaAddr8),
    ("OR", 0x14, AddrMode::RegCommaAddr8PlusReg(Reg::A, Reg::X)),
    ("OR", 0x15, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::X)),
    ("OR", 0x16, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::Y)),
    ("OR", 0x17, AddrMode::RegCommaBracAddr8KetsPlusReg(Reg::A, Reg::Y)),
    ("OR", 0x18, AddrMode::Addr8CommaPoundImm8),
    ("OR", 0x19, AddrMode::ParRegEnsCommaParRegEns(Reg::X, Reg::Y)),
    ("PCALL", 0x4f, AddrMode::AddrHi),
    ("POP", 0x8e, AddrMode::Reg(Reg::Psw)),
    ("POP", 0xae, AddrMode::Reg(Reg::A)),
    ("POP", 0xce, AddrMode::Reg(Reg::X)),
    ("POP", 0xee, AddrMode::Reg(Reg::Y)),
    ("PUSH", 0x0d, AddrMode::Reg(Reg::Psw)),
    ("PUSH", 0x2d, AddrMode::Reg(Reg::A)),
    ("PUSH", 0x4d, AddrMode::Reg(Reg::X)),
    ("PUSH", 0x6d, AddrMode::Reg(Reg::Y)),
    ("RET", 0x6f, AddrMode::Implied),
    ("RETI", 0x7f, AddrMode::Implied),
    ("ROL", 0x2b, AddrMode::Addr8),
    ("ROL", 0x2c, AddrMode::BangAddr16),
    ("ROL", 0x3b, AddrMode::Addr8PlusReg(Reg::X)),
    ("ROL", 0x3c, AddrMode::Reg(Reg::A)),
    ("ROR", 0x6b, AddrMode::Addr8),
    ("ROR", 0x6c, AddrMode::BangAddr16),
    ("ROR", 0x7b, AddrMode::Addr8PlusReg(Reg::X)),
    ("ROR", 0x7c, AddrMode::Reg(Reg::A)),
    ("SBC", 0xa4, AddrMode::RegCommaAddr8(Reg::A)),
    ("SBC", 0xa5, AddrMode::RegCommaBangAddr16(Reg::A)),
    ("SBC", 0xa6, AddrMode::RegCommaParRegEns(Reg::A, Reg::X)),
    ("SBC", 0xa7, AddrMode::RegCommaBracAddr8PlusRegKets(Reg::A, Reg::X)),
    ("SBC", 0xa8, AddrMode::RegCommaPoundImm8(Reg::A)),
    ("SBC", 0xa9, AddrMode::Addr8CommaAddr8),
    ("SBC", 0xb4, AddrMode::RegCommaAddr8PlusReg(Reg::A, Reg::X)),
    ("SBC", 0xb5, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::X)),
    ("SBC", 0xb6, AddrMode::RegCommaBangAddr16PlusReg(Reg::A, Reg::Y)),
    ("SBC", 0xb7, AddrMode::RegCommaBracAddr8KetsPlusReg(Reg::A, Reg::Y)),
    ("SBC", 0xb8, AddrMode::Addr8CommaPoundImm8),
    ("SBC", 0xb9, AddrMode::ParRegEnsCommaParRegEns(Reg::X, Reg::Y)),
    ("SETC", 0x80, AddrMode::Implied),
    ("SETP", 0x40, AddrMode::Implied),
    ("SLEEP", 0xef, AddrMode::Implied),
    ("STOP", 0xff, AddrMode::Implied),
    ("SUBW", 0x9a, AddrMode::RegCommaAddr8(Reg::Ya)),
    ("TCLR1", 0x4e, AddrMode::BangAddr16),
    ("TSET1", 0x0e, AddrMode::BangAddr16),
    ("XCN", 0x9f, AddrMode::Reg(Reg::A)),
];

const MACROS_SUPERFX: &[(&str, u8, AddrMode)] = &[
    ("ADD", 0x50, AddrMode::Reg(Reg::R0)),
    ("ADD", 0x51, AddrMode::Reg(Reg::R1)),
    ("ADD", 0x52, AddrMode::Reg(Reg::R2)),
    ("ADD", 0x53, AddrMode::Reg(Reg::R3)),
    ("ADD", 0x54, AddrMode::Reg(Reg::R4)),
    ("ADD", 0x55, AddrMode::Reg(Reg::R5)),
    ("ADD", 0x56, AddrMode::Reg(Reg::R6)),
    ("ADD", 0x57, AddrMode::Reg(Reg::R7)),
    ("ADD", 0x58, AddrMode::Reg(Reg::R8)),
    ("ADD", 0x59, AddrMode::Reg(Reg::R9)),
    ("ADD", 0x5a, AddrMode::Reg(Reg::R10)),
    ("ADD", 0x5b, AddrMode::Reg(Reg::R11)),
    ("ADD", 0x5c, AddrMode::Reg(Reg::R12)),
    ("ADD", 0x5d, AddrMode::Reg(Reg::R13)),
    ("ADD", 0x5e, AddrMode::Reg(Reg::R14)),
    ("ADD", 0x5f, AddrMode::Reg(Reg::R15)),
    ("ALT1", 0x3d, AddrMode::Implied),
    ("ALT2", 0x3e, AddrMode::Implied),
    ("ALT3", 0x3f, AddrMode::Implied),
    ("AND", 0x71, AddrMode::Reg(Reg::R1)),
    ("AND", 0x72, AddrMode::Reg(Reg::R2)),
    ("AND", 0x73, AddrMode::Reg(Reg::R3)),
    ("AND", 0x74, AddrMode::Reg(Reg::R4)),
    ("AND", 0x75, AddrMode::Reg(Reg::R5)),
    ("AND", 0x76, AddrMode::Reg(Reg::R6)),
    ("AND", 0x77, AddrMode::Reg(Reg::R7)),
    ("AND", 0x78, AddrMode::Reg(Reg::R8)),
    ("AND", 0x79, AddrMode::Reg(Reg::R9)),
    ("AND", 0x7a, AddrMode::Reg(Reg::R10)),
    ("AND", 0x7b, AddrMode::Reg(Reg::R11)),
    ("AND", 0x7c, AddrMode::Reg(Reg::R12)),
    ("AND", 0x7d, AddrMode::Reg(Reg::R13)),
    ("AND", 0x7e, AddrMode::Reg(Reg::R14)),
    ("AND", 0x7f, AddrMode::Reg(Reg::R15)),
    ("ASR", 0x96, AddrMode::Implied),
    ("BCC", 0x0c, AddrMode::Branch),
    ("BCS", 0x0d, AddrMode::Branch),
    ("BEQ", 0x09, AddrMode::Branch),
    ("BGE", 0x06, AddrMode::Branch),
    ("BLT", 0x07, AddrMode::Branch),
    ("BMI", 0x0b, AddrMode::Branch),
    ("BNE", 0x08, AddrMode::Branch),
    ("BPL", 0x0a, AddrMode::Branch),
    ("BRA", 0x05, AddrMode::Branch),
    ("BVC", 0x0e, AddrMode::Branch),
    ("BVS", 0x0f, AddrMode::Branch),
    ("CACHE", 0x02, AddrMode::Implied),
    ("COLOR", 0x4e, AddrMode::Implied),
    ("DEC", 0xe0, AddrMode::Reg(Reg::R0)),
    ("DEC", 0xe1, AddrMode::Reg(Reg::R1)),
    ("DEC", 0xe2, AddrMode::Reg(Reg::R2)),
    ("DEC", 0xe3, AddrMode::Reg(Reg::R3)),
    ("DEC", 0xe4, AddrMode::Reg(Reg::R4)),
    ("DEC", 0xe5, AddrMode::Reg(Reg::R5)),
    ("DEC", 0xe6, AddrMode::Reg(Reg::R6)),
    ("DEC", 0xe7, AddrMode::Reg(Reg::R7)),
    ("DEC", 0xe8, AddrMode::Reg(Reg::R8)),
    ("DEC", 0xe9, AddrMode::Reg(Reg::R9)),
    ("DEC", 0xea, AddrMode::Reg(Reg::R10)),
    ("DEC", 0xeb, AddrMode::Reg(Reg::R11)),
    ("DEC", 0xec, AddrMode::Reg(Reg::R12)),
    ("DEC", 0xed, AddrMode::Reg(Reg::R13)),
    ("DEC", 0xee, AddrMode::Reg(Reg::R14)),
    ("FMULT", 0x9f, AddrMode::Implied),
    ("FROM", 0xb0, AddrMode::Reg(Reg::R0)),
    ("FROM", 0xb1, AddrMode::Reg(Reg::R1)),
    ("FROM", 0xb2, AddrMode::Reg(Reg::R2)),
    ("FROM", 0xb3, AddrMode::Reg(Reg::R3)),
    ("FROM", 0xb4, AddrMode::Reg(Reg::R4)),
    ("FROM", 0xb5, AddrMode::Reg(Reg::R5)),
    ("FROM", 0xb6, AddrMode::Reg(Reg::R6)),
    ("FROM", 0xb7, AddrMode::Reg(Reg::R7)),
    ("FROM", 0xb8, AddrMode::Reg(Reg::R8)),
    ("FROM", 0xb9, AddrMode::Reg(Reg::R9)),
    ("FROM", 0xba, AddrMode::Reg(Reg::R10)),
    ("FROM", 0xbb, AddrMode::Reg(Reg::R11)),
    ("FROM", 0xbc, AddrMode::Reg(Reg::R12)),
    ("FROM", 0xbd, AddrMode::Reg(Reg::R13)),
    ("FROM", 0xbe, AddrMode::Reg(Reg::R14)),
    ("FROM", 0xbf, AddrMode::Reg(Reg::R15)),
    ("GETB", 0xef, AddrMode::Implied),
    ("GETC", 0xdf, AddrMode::Implied),
    ("HIB", 0xc0, AddrMode::Implied),
    ("IBT", 0xa0, AddrMode::RegCommaPoundImm8(Reg::R0)),
    ("IBT", 0xa1, AddrMode::RegCommaPoundImm8(Reg::R1)),
    ("IBT", 0xa2, AddrMode::RegCommaPoundImm8(Reg::R2)),
    ("IBT", 0xa3, AddrMode::RegCommaPoundImm8(Reg::R3)),
    ("IBT", 0xa4, AddrMode::RegCommaPoundImm8(Reg::R4)),
    ("IBT", 0xa5, AddrMode::RegCommaPoundImm8(Reg::R5)),
    ("IBT", 0xa6, AddrMode::RegCommaPoundImm8(Reg::R6)),
    ("IBT", 0xa7, AddrMode::RegCommaPoundImm8(Reg::R7)),
    ("IBT", 0xa8, AddrMode::RegCommaPoundImm8(Reg::R8)),
    ("IBT", 0xa9, AddrMode::RegCommaPoundImm8(Reg::R9)),
    ("IBT", 0xaa, AddrMode::RegCommaPoundImm8(Reg::R10)),
    ("IBT", 0xab, AddrMode::RegCommaPoundImm8(Reg::R11)),
    ("IBT", 0xac, AddrMode::RegCommaPoundImm8(Reg::R12)),
    ("IBT", 0xad, AddrMode::RegCommaPoundImm8(Reg::R13)),
    ("IBT", 0xae, AddrMode::RegCommaPoundImm8(Reg::R14)),
    ("IBT", 0xaf, AddrMode::RegCommaPoundImm8(Reg::R15)),
    ("INC", 0xd0, AddrMode::Reg(Reg::R0)),
    ("INC", 0xd1, AddrMode::Reg(Reg::R1)),
    ("INC", 0xd2, AddrMode::Reg(Reg::R2)),
    ("INC", 0xd3, AddrMode::Reg(Reg::R3)),
    ("INC", 0xd4, AddrMode::Reg(Reg::R4)),
    ("INC", 0xd5, AddrMode::Reg(Reg::R5)),
    ("INC", 0xd6, AddrMode::Reg(Reg::R6)),
    ("INC", 0xd7, AddrMode::Reg(Reg::R7)),
    ("INC", 0xd8, AddrMode::Reg(Reg::R8)),
    ("INC", 0xd9, AddrMode::Reg(Reg::R9)),
    ("INC", 0xda, AddrMode::Reg(Reg::R10)),
    ("INC", 0xdb, AddrMode::Reg(Reg::R11)),
    ("INC", 0xdc, AddrMode::Reg(Reg::R12)),
    ("INC", 0xdd, AddrMode::Reg(Reg::R13)),
    ("INC", 0xde, AddrMode::Reg(Reg::R14)),
    ("IWT", 0xf0, AddrMode::RegCommaPoundImm16(Reg::R0)),
    ("IWT", 0xf1, AddrMode::RegCommaPoundImm16(Reg::R1)),
    ("IWT", 0xf2, AddrMode::RegCommaPoundImm16(Reg::R2)),
    ("IWT", 0xf3, AddrMode::RegCommaPoundImm16(Reg::R3)),
    ("IWT", 0xf4, AddrMode::RegCommaPoundImm16(Reg::R4)),
    ("IWT", 0xf5, AddrMode::RegCommaPoundImm16(Reg::R5)),
    ("IWT", 0xf6, AddrMode::RegCommaPoundImm16(Reg::R6)),
    ("IWT", 0xf7, AddrMode::RegCommaPoundImm16(Reg::R7)),
    ("IWT", 0xf8, AddrMode::RegCommaPoundImm16(Reg::R8)),
    ("IWT", 0xf9, AddrMode::RegCommaPoundImm16(Reg::R9)),
    ("IWT", 0xfa, AddrMode::RegCommaPoundImm16(Reg::R10)),
    ("IWT", 0xfb, AddrMode::RegCommaPoundImm16(Reg::R11)),
    ("IWT", 0xfc, AddrMode::RegCommaPoundImm16(Reg::R12)),
    ("IWT", 0xfd, AddrMode::RegCommaPoundImm16(Reg::R13)),
    ("IWT", 0xfe, AddrMode::RegCommaPoundImm16(Reg::R14)),
    ("IWT", 0xff, AddrMode::RegCommaPoundImm16(Reg::R15)),
    ("JMP", 0x98, AddrMode::Reg(Reg::R8)),
    ("JMP", 0x99, AddrMode::Reg(Reg::R9)),
    ("JMP", 0x9a, AddrMode::Reg(Reg::R10)),
    ("JMP", 0x9b, AddrMode::Reg(Reg::R11)),
    ("JMP", 0x9c, AddrMode::Reg(Reg::R12)),
    ("JMP", 0x9d, AddrMode::Reg(Reg::R13)),
    ("LDW", 0x40, AddrMode::ParRegEns(Reg::R0)),
    ("LDW", 0x41, AddrMode::ParRegEns(Reg::R1)),
    ("LDW", 0x42, AddrMode::ParRegEns(Reg::R2)),
    ("LDW", 0x43, AddrMode::ParRegEns(Reg::R3)),
    ("LDW", 0x44, AddrMode::ParRegEns(Reg::R4)),
    ("LDW", 0x45, AddrMode::ParRegEns(Reg::R5)),
    ("LDW", 0x46, AddrMode::ParRegEns(Reg::R6)),
    ("LDW", 0x47, AddrMode::ParRegEns(Reg::R7)),
    ("LDW", 0x48, AddrMode::ParRegEns(Reg::R8)),
    ("LDW", 0x49, AddrMode::ParRegEns(Reg::R9)),
    ("LDW", 0x4a, AddrMode::ParRegEns(Reg::R10)),
    ("LDW", 0x4b, AddrMode::ParRegEns(Reg::R11)),
    ("LINK", 0x91, AddrMode::SuperFxLink),
    ("LOB", 0x9e, AddrMode::Implied),
    ("LOOP", 0x3c, AddrMode::Implied),
    ("LSR", 0x03, AddrMode::Implied),
    ("MERGE", 0x70, AddrMode::Implied),
    ("MULT", 0x80, AddrMode::Reg(Reg::R0)),
    ("MULT", 0x81, AddrMode::Reg(Reg::R1)),
    ("MULT", 0x82, AddrMode::Reg(Reg::R2)),
    ("MULT", 0x83, AddrMode::Reg(Reg::R3)),
    ("MULT", 0x84, AddrMode::Reg(Reg::R4)),
    ("MULT", 0x85, AddrMode::Reg(Reg::R5)),
    ("MULT", 0x86, AddrMode::Reg(Reg::R6)),
    ("MULT", 0x87, AddrMode::Reg(Reg::R7)),
    ("MULT", 0x88, AddrMode::Reg(Reg::R8)),
    ("MULT", 0x89, AddrMode::Reg(Reg::R9)),
    ("MULT", 0x8a, AddrMode::Reg(Reg::R10)),
    ("MULT", 0x8b, AddrMode::Reg(Reg::R11)),
    ("MULT", 0x8c, AddrMode::Reg(Reg::R12)),
    ("MULT", 0x8d, AddrMode::Reg(Reg::R13)),
    ("MULT", 0x8e, AddrMode::Reg(Reg::R14)),
    ("MULT", 0x8f, AddrMode::Reg(Reg::R15)),
    ("NOP", 0x01, AddrMode::Implied),
    ("NOT", 0x4f, AddrMode::Implied),
    ("OR", 0xc1, AddrMode::Reg(Reg::R1)),
    ("OR", 0xc2, AddrMode::Reg(Reg::R2)),
    ("OR", 0xc3, AddrMode::Reg(Reg::R3)),
    ("OR", 0xc4, AddrMode::Reg(Reg::R4)),
    ("OR", 0xc5, AddrMode::Reg(Reg::R5)),
    ("OR", 0xc6, AddrMode::Reg(Reg::R6)),
    ("OR", 0xc7, AddrMode::Reg(Reg::R7)),
    ("OR", 0xc8, AddrMode::Reg(Reg::R8)),
    ("OR", 0xc9, AddrMode::Reg(Reg::R9)),
    ("OR", 0xca, AddrMode::Reg(Reg::R10)),
    ("OR", 0xcb, AddrMode::Reg(Reg::R11)),
    ("OR", 0xcc, AddrMode::Reg(Reg::R12)),
    ("OR", 0xcd, AddrMode::Reg(Reg::R13)),
    ("OR", 0xce, AddrMode::Reg(Reg::R14)),
    ("OR", 0xcf, AddrMode::Reg(Reg::R15)),
    ("PLOT", 0x4c, AddrMode::Implied),
    ("ROL", 0x04, AddrMode::Implied),
    ("ROR", 0x97, AddrMode::Implied),
    ("SBK", 0x90, AddrMode::Implied),
    ("SEX", 0x95, AddrMode::Implied),
    ("STOP", 0x00, AddrMode::Implied),
    ("STW", 0x30, AddrMode::ParRegEns(Reg::R0)),
    ("STW", 0x31, AddrMode::ParRegEns(Reg::R1)),
    ("STW", 0x32, AddrMode::ParRegEns(Reg::R2)),
    ("STW", 0x33, AddrMode::ParRegEns(Reg::R3)),
    ("STW", 0x34, AddrMode::ParRegEns(Reg::R4)),
    ("STW", 0x35, AddrMode::ParRegEns(Reg::R5)),
    ("STW", 0x36, AddrMode::ParRegEns(Reg::R6)),
    ("STW", 0x37, AddrMode::ParRegEns(Reg::R7)),
    ("STW", 0x38, AddrMode::ParRegEns(Reg::R8)),
    ("STW", 0x39, AddrMode::ParRegEns(Reg::R9)),
    ("STW", 0x3a, AddrMode::ParRegEns(Reg::R10)),
    ("STW", 0x3b, AddrMode::ParRegEns(Reg::R11)),
    ("SUB", 0x60, AddrMode::Reg(Reg::R0)),
    ("SUB", 0x61, AddrMode::Reg(Reg::R1)),
    ("SUB", 0x62, AddrMode::Reg(Reg::R2)),
    ("SUB", 0x63, AddrMode::Reg(Reg::R3)),
    ("SUB", 0x64, AddrMode::Reg(Reg::R4)),
    ("SUB", 0x65, AddrMode::Reg(Reg::R5)),
    ("SUB", 0x66, AddrMode::Reg(Reg::R6)),
    ("SUB", 0x67, AddrMode::Reg(Reg::R7)),
    ("SUB", 0x68, AddrMode::Reg(Reg::R8)),
    ("SUB", 0x69, AddrMode::Reg(Reg::R9)),
    ("SUB", 0x6a, AddrMode::Reg(Reg::R10)),
    ("SUB", 0x6b, AddrMode::Reg(Reg::R11)),
    ("SUB", 0x6c, AddrMode::Reg(Reg::R12)),
    ("SUB", 0x6d, AddrMode::Reg(Reg::R13)),
    ("SUB", 0x6e, AddrMode::Reg(Reg::R14)),
    ("SUB", 0x6f, AddrMode::Reg(Reg::R15)),
    ("SWAP", 0x4d, AddrMode::Implied),
    ("TO", 0x10, AddrMode::Reg(Reg::R0)),
    ("TO", 0x11, AddrMode::Reg(Reg::R1)),
    ("TO", 0x12, AddrMode::Reg(Reg::R2)),
    ("TO", 0x13, AddrMode::Reg(Reg::R3)),
    ("TO", 0x14, AddrMode::Reg(Reg::R4)),
    ("TO", 0x15, AddrMode::Reg(Reg::R5)),
    ("TO", 0x16, AddrMode::Reg(Reg::R6)),
    ("TO", 0x17, AddrMode::Reg(Reg::R7)),
    ("TO", 0x18, AddrMode::Reg(Reg::R8)),
    ("TO", 0x19, AddrMode::Reg(Reg::R9)),
    ("TO", 0x1a, AddrMode::Reg(Reg::R10)),
    ("TO", 0x1b, AddrMode::Reg(Reg::R11)),
    ("TO", 0x1c, AddrMode::Reg(Reg::R12)),
    ("TO", 0x1d, AddrMode::Reg(Reg::R13)),
    ("TO", 0x1e, AddrMode::Reg(Reg::R14)),
    ("TO", 0x1f, AddrMode::Reg(Reg::R15)),
    ("WITH", 0x20, AddrMode::Reg(Reg::R0)),
    ("WITH", 0x21, AddrMode::Reg(Reg::R1)),
    ("WITH", 0x22, AddrMode::Reg(Reg::R2)),
    ("WITH", 0x23, AddrMode::Reg(Reg::R3)),
    ("WITH", 0x24, AddrMode::Reg(Reg::R4)),
    ("WITH", 0x25, AddrMode::Reg(Reg::R5)),
    ("WITH", 0x26, AddrMode::Reg(Reg::R6)),
    ("WITH", 0x27, AddrMode::Reg(Reg::R7)),
    ("WITH", 0x28, AddrMode::Reg(Reg::R8)),
    ("WITH", 0x29, AddrMode::Reg(Reg::R9)),
    ("WITH", 0x2a, AddrMode::Reg(Reg::R10)),
    ("WITH", 0x2b, AddrMode::Reg(Reg::R11)),
    ("WITH", 0x2c, AddrMode::Reg(Reg::R12)),
    ("WITH", 0x2d, AddrMode::Reg(Reg::R13)),
    ("WITH", 0x2e, AddrMode::Reg(Reg::R14)),
    ("WITH", 0x2f, AddrMode::Reg(Reg::R15)),
];

//===========================================================================//

#[derive(Clone, Copy)]
enum AddrMode {
    /// FOO addr
    Addr8,
    /// FOO addr1, addr2
    Addr8CommaAddr8,
    /// FOO addr, #imm
    Addr8CommaPoundImm8,
    /// FOO addr + R
    Addr8PlusReg(Reg),
    /// FOO addr + R1, R2
    Addr8PlusRegCommaReg(Reg, Reg),
    /// FOO addr, R
    Addr8CommaReg(Reg),
    /// FOO addr
    Addr16,
    /// FOO addr
    AddrHi,
    /// FOO !addr
    BangAddr16,
    /// FOO !addr, R
    BangAddr16CommaReg(Reg),
    /// FOO !addr + R1, R2
    BangAddr16PlusRegCommaReg(Reg, Reg),
    /// FOO !!addr
    BangBangAddr24,
    /// FOO !!addr, R
    BangBangAddr24CommaReg(Reg),
    /// FOO [addr]
    BracAddr8Kets,
    /// FOO [addr], R
    BracAddr8KetsCommaReg(Reg),
    /// FOO [addr] + R1, R2
    BracAddr8KetsPlusRegCommaReg(Reg, Reg),
    /// FOO [addr + R1], R2
    BracAddr8PlusRegKetsCommaReg(Reg, Reg),
    /// FOO [!addr]
    BracBangAddr16Kets,
    /// FOO [!addr + R]
    BracBangAddr16PlusRegKets(Reg),
    /// FOO addr
    Branch,
    /// FOO (addr, R)
    ParAddr8CommaRegEns(Reg),
    /// FOO (addr, R1), R2
    ParAddr8CommaRegEnsCommaReg(Reg, Reg),
    /// FOO (addr)
    ParAddr8Ens,
    /// FOO (addr), R
    ParAddr8EnsCommaReg(Reg),
    /// FOO (!addr, R)
    ParBangAddr16CommaRegEns(Reg),
    /// FOO (!addr)
    ParBangAddr16Ens,
    /// FOO (R)
    ParRegEns(Reg),
    /// FOO (R1), (R2)
    ParRegEnsCommaParRegEns(Reg, Reg),
    /// FOO (R1), R2
    ParRegEnsCommaReg(Reg, Reg),
    /// FOO (R1)+, R2
    ParRegEnsPlusCommaReg(Reg, Reg),
    /// FOO #imm
    PoundImm8,
    /// FOO #imm, #imm2
    PoundImm8CommaPoundImm8,
    /// FOO ##imm
    PoundPoundImm16,
    /// FOO
    Implied,
    /// FOO R
    Reg(Reg),
    /// FOO R, addr
    RegCommaAddr8(Reg),
    /// FOO R1, addr + R2
    RegCommaAddr8PlusReg(Reg, Reg),
    /// FOO R, !addr
    RegCommaBangAddr16(Reg),
    /// FOO R1, !addr + R2
    RegCommaBangAddr16PlusReg(Reg, Reg),
    /// FOO R1, [addr] + R2
    RegCommaBracAddr8KetsPlusReg(Reg, Reg),
    /// FOO R1, [addr + R2]
    RegCommaBracAddr8PlusRegKets(Reg, Reg),
    /// FOO R1, (R2)
    RegCommaParRegEns(Reg, Reg),
    /// FOO R1, (R2)+
    RegCommaParRegEnsPlus(Reg, Reg),
    /// FOO R1, #imm
    RegCommaPoundImm8(Reg),
    /// FOO R1, #imm
    RegCommaPoundImm16(Reg),
    /// FOO R1, R2
    RegCommaReg(Reg, Reg),
    /// FOO #imm
    SuperFxLink,
}

/// Register names used across various architectures.
#[derive(Clone, Copy)]
enum Reg {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Psw,
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    S,
    Sp,
    X,
    Y,
    Ya,
}

//===========================================================================//

pub(super) fn make_builtins() -> (ArchTree, MacroTable) {
    let mut arch_tree = ArchTree::new();

    let arch_65xx = Rc::<str>::from("65xx");
    let arch_6502 = Rc::<str>::from("6502");
    let arch_65c816 = Rc::<str>::from("65C816");
    let arch_sm83 = Rc::<str>::from("SM83");
    let arch_spc700 = Rc::<str>::from("SPC700");
    let arch_superfx = Rc::<str>::from("SuperFX");

    let reg_a = Rc::<str>::from("A");
    let reg_af = Rc::<str>::from("AF");
    let reg_b = Rc::<str>::from("B");
    let reg_bc = Rc::<str>::from("BC");
    let reg_c = Rc::<str>::from("C");
    let reg_d = Rc::<str>::from("D");
    let reg_de = Rc::<str>::from("DE");
    let reg_e = Rc::<str>::from("E");
    let reg_h = Rc::<str>::from("H");
    let reg_hl = Rc::<str>::from("HL");
    let reg_l = Rc::<str>::from("L");
    let reg_nc = Rc::<str>::from("NC");
    let reg_nz = Rc::<str>::from("NZ");
    let reg_psw = Rc::<str>::from("PSW");
    let reg_r0 = Rc::<str>::from("R0");
    let reg_r1 = Rc::<str>::from("R1");
    let reg_r2 = Rc::<str>::from("R2");
    let reg_r3 = Rc::<str>::from("R3");
    let reg_r4 = Rc::<str>::from("R4");
    let reg_r5 = Rc::<str>::from("R5");
    let reg_r6 = Rc::<str>::from("R6");
    let reg_r7 = Rc::<str>::from("R7");
    let reg_r8 = Rc::<str>::from("R8");
    let reg_r9 = Rc::<str>::from("R9");
    let reg_r10 = Rc::<str>::from("R10");
    let reg_r11 = Rc::<str>::from("R11");
    let reg_r12 = Rc::<str>::from("R12");
    let reg_r13 = Rc::<str>::from("R13");
    let reg_r14 = Rc::<str>::from("R14");
    let reg_r15 = Rc::<str>::from("R15");
    let reg_s = Rc::<str>::from("S");
    let reg_sp = Rc::<str>::from("SP");
    let reg_x = Rc::<str>::from("X");
    let reg_y = Rc::<str>::from("Y");
    let reg_ya = Rc::<str>::from("YA");
    let reg_z = Rc::<str>::from("Z");

    let res_65xx = vec![reg_a.clone(), reg_x.clone(), reg_y.clone()];
    let res_65c816 = vec![reg_s.clone()];
    let res_sm83 = vec![
        reg_a.clone(),
        reg_af.clone(),
        reg_b.clone(),
        reg_bc.clone(),
        reg_c.clone(),
        reg_d.clone(),
        reg_de.clone(),
        reg_e.clone(),
        reg_h.clone(),
        reg_hl.clone(),
        reg_l.clone(),
        reg_nc.clone(),
        reg_nz.clone(),
        reg_sp.clone(),
        reg_z.clone(),
    ];
    let res_spc700 = vec![
        reg_a.clone(),
        reg_c.clone(),
        reg_psw.clone(),
        reg_sp.clone(),
        reg_x.clone(),
        reg_y.clone(),
        reg_ya.clone(),
    ];
    let res_superfx = vec![
        reg_r0.clone(),
        reg_r1.clone(),
        reg_r2.clone(),
        reg_r3.clone(),
        reg_r4.clone(),
        reg_r5.clone(),
        reg_r6.clone(),
        reg_r7.clone(),
        reg_r8.clone(),
        reg_r9.clone(),
        reg_r10.clone(),
        reg_r11.clone(),
        reg_r12.clone(),
        reg_r13.clone(),
        reg_r14.clone(),
        reg_r15.clone(),
    ];

    arch_tree
        .define_arch(
            arch_65xx.clone(),
            ArchTree::ROOT_ARCH_NAME,
            &res_65xx,
            Some(Endianness::LittleEndian),
        )
        .unwrap();
    arch_tree
        .define_arch(
            arch_6502.clone(),
            &arch_65xx,
            &[],
            Some(Endianness::LittleEndian),
        )
        .unwrap();
    arch_tree
        .define_arch(
            arch_65c816.clone(),
            &arch_65xx,
            &res_65c816,
            Some(Endianness::LittleEndian),
        )
        .unwrap();
    arch_tree
        .define_arch(
            arch_sm83.clone(),
            ArchTree::ROOT_ARCH_NAME,
            &res_sm83,
            Some(Endianness::LittleEndian),
        )
        .unwrap();
    arch_tree
        .define_arch(
            arch_spc700.clone(),
            ArchTree::ROOT_ARCH_NAME,
            &res_spc700,
            Some(Endianness::LittleEndian),
        )
        .unwrap();
    arch_tree
        .define_arch(
            arch_superfx.clone(),
            ArchTree::ROOT_ARCH_NAME,
            &res_superfx,
            Some(Endianness::LittleEndian),
        )
        .unwrap();

    let mut builder = BuiltinBuilder {
        arch_tree,
        macros: MacroTable::new(),
        placeholder_addr: Rc::from("%ADDR"),
        placeholder_addr2: Rc::from("%ADDR2"),
        placeholder_imm: Rc::from("%IMM"),
        placeholder_imm2: Rc::from("%IMM2"),
        reg_a,
        reg_b,
        reg_c,
        reg_d,
        reg_e,
        reg_h,
        reg_l,
        reg_psw,
        reg_r0,
        reg_r1,
        reg_r2,
        reg_r3,
        reg_r4,
        reg_r5,
        reg_r6,
        reg_r7,
        reg_r8,
        reg_r9,
        reg_r10,
        reg_r11,
        reg_r12,
        reg_r13,
        reg_r14,
        reg_r15,
        reg_s,
        reg_sp,
        reg_x,
        reg_y,
        reg_ya,
    };
    builder.add_macros(&arch_65xx, MACROS_65XX);
    builder.add_macros(&arch_6502, MACROS_6502);
    builder.add_macros(&arch_65c816, MACROS_65C816);
    builder.add_macros(&arch_sm83, MACROS_SM83);
    builder.add_macros(&arch_spc700, MACROS_SPC700);
    builder.add_macros(&arch_superfx, MACROS_SUPERFX);
    (builder.arch_tree, builder.macros)
}

//===========================================================================//

struct BuiltinBuilder {
    arch_tree: ArchTree,
    macros: MacroTable,
    placeholder_addr: Rc<str>,
    placeholder_addr2: Rc<str>,
    placeholder_imm: Rc<str>,
    placeholder_imm2: Rc<str>,
    reg_a: Rc<str>,
    reg_b: Rc<str>,
    reg_c: Rc<str>,
    reg_d: Rc<str>,
    reg_e: Rc<str>,
    reg_h: Rc<str>,
    reg_l: Rc<str>,
    reg_psw: Rc<str>,
    reg_r0: Rc<str>,
    reg_r1: Rc<str>,
    reg_r2: Rc<str>,
    reg_r3: Rc<str>,
    reg_r4: Rc<str>,
    reg_r5: Rc<str>,
    reg_r6: Rc<str>,
    reg_r7: Rc<str>,
    reg_r8: Rc<str>,
    reg_r9: Rc<str>,
    reg_r10: Rc<str>,
    reg_r11: Rc<str>,
    reg_r12: Rc<str>,
    reg_r13: Rc<str>,
    reg_r14: Rc<str>,
    reg_r15: Rc<str>,
    reg_s: Rc<str>,
    reg_sp: Rc<str>,
    reg_x: Rc<str>,
    reg_y: Rc<str>,
    reg_ya: Rc<str>,
}

impl BuiltinBuilder {
    fn add_macros(&mut self, arch: &Rc<str>, macros: &[(&str, u8, AddrMode)]) {
        for &(name, opcode, addr_mode) in macros {
            self.add_macro(arch, name, opcode, addr_mode);
        }
    }

    fn add_macro(
        &mut self,
        arch: &Rc<str>,
        name: &str,
        opcode_byte: u8,
        addr_mode: AddrMode,
    ) {
        let params = match addr_mode {
            AddrMode::Addr8
            | AddrMode::Addr16
            | AddrMode::AddrHi
            | AddrMode::Branch => {
                vec![self.addr_arg()]
            }
            AddrMode::Addr8CommaAddr8 => {
                vec![self.addr_arg(), self.addr2_arg()]
            }
            AddrMode::Addr8PlusReg(r1) => {
                vec![self.addr_plus_reg_arg(r1)]
            }
            AddrMode::Addr8PlusRegCommaReg(r1, r2) => {
                vec![self.addr_plus_reg_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::Addr8CommaPoundImm8 => {
                vec![self.addr_arg(), self.pound_imm_arg()]
            }
            AddrMode::Addr8CommaReg(r1) => {
                vec![self.addr_arg(), self.reg_arg(r1)]
            }
            AddrMode::BangAddr16 => vec![self.bang_addr_arg()],
            AddrMode::BangAddr16PlusRegCommaReg(r1, r2) => {
                vec![self.bang_addr_plus_reg_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::BangAddr16CommaReg(r1) => {
                vec![self.bang_addr_arg(), self.reg_arg(r1)]
            }
            AddrMode::BangBangAddr24 => vec![self.bang_bang_addr_arg()],
            AddrMode::BangBangAddr24CommaReg(r1) => {
                vec![self.bang_bang_addr_arg(), self.reg_arg(r1)]
            }
            AddrMode::BracAddr8Kets => vec![self.brac_addr_kets_arg()],
            AddrMode::BracAddr8KetsCommaReg(r1) => {
                vec![self.brac_addr_kets_arg(), self.reg_arg(r1)]
            }
            AddrMode::BracAddr8KetsPlusRegCommaReg(r1, r2) => {
                vec![self.brac_addr_kets_plus_reg_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::BracAddr8PlusRegKetsCommaReg(r1, r2) => {
                vec![self.brac_addr_plus_reg_kets_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::BracBangAddr16Kets => {
                vec![self.brac_bang_addr_kets_arg()]
            }
            AddrMode::BracBangAddr16PlusRegKets(r1) => {
                vec![self.brac_bang_addr_plus_reg_kets_arg(r1)]
            }
            AddrMode::Implied => vec![],
            AddrMode::ParAddr8CommaRegEns(r1) => {
                vec![self.par_addr_comma_reg_ens_arg(r1)]
            }
            AddrMode::ParAddr8CommaRegEnsCommaReg(r1, r2) => {
                vec![self.par_addr_comma_reg_ens_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::ParAddr8Ens => vec![self.par_addr_ens_arg()],
            AddrMode::ParAddr8EnsCommaReg(r1) => {
                vec![self.par_addr_ens_arg(), self.reg_arg(r1)]
            }
            AddrMode::ParBangAddr16CommaRegEns(r1) => {
                vec![self.par_bang_addr_comma_reg_ens_arg(r1)]
            }
            AddrMode::ParBangAddr16Ens => vec![self.par_bang_addr_ens_arg()],
            AddrMode::ParRegEns(r1) => vec![self.par_reg_ens_arg(r1)],
            AddrMode::ParRegEnsCommaParRegEns(r1, r2) => {
                vec![self.par_reg_ens_arg(r1), self.par_reg_ens_arg(r2)]
            }
            AddrMode::ParRegEnsPlusCommaReg(r1, r2) => {
                vec![self.par_reg_ens_plus_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::ParRegEnsCommaReg(r1, r2) => {
                vec![self.par_reg_ens_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::PoundImm8 | AddrMode::SuperFxLink => {
                vec![self.pound_imm_arg()]
            }
            AddrMode::PoundImm8CommaPoundImm8 => {
                vec![self.pound_imm_arg(), self.pound_imm2_arg()]
            }
            AddrMode::PoundPoundImm16 => vec![self.pound_pound_imm_arg()],
            AddrMode::Reg(r1) => vec![self.reg_arg(r1)],
            AddrMode::RegCommaAddr8(r1) => {
                vec![self.reg_arg(r1), self.addr_arg()]
            }
            AddrMode::RegCommaAddr8PlusReg(r1, r2) => {
                vec![self.reg_arg(r1), self.addr_plus_reg_arg(r2)]
            }
            AddrMode::RegCommaBangAddr16(r1) => {
                vec![self.reg_arg(r1), self.bang_addr_arg()]
            }
            AddrMode::RegCommaBangAddr16PlusReg(r1, r2) => {
                vec![self.reg_arg(r1), self.bang_addr_plus_reg_arg(r2)]
            }
            AddrMode::RegCommaBracAddr8KetsPlusReg(r1, r2) => {
                vec![self.reg_arg(r1), self.brac_addr_kets_plus_reg_arg(r2)]
            }
            AddrMode::RegCommaBracAddr8PlusRegKets(r1, r2) => {
                vec![self.reg_arg(r1), self.brac_addr_plus_reg_kets_arg(r2)]
            }
            AddrMode::RegCommaParRegEns(r1, r2) => {
                vec![self.reg_arg(r1), self.par_reg_ens_arg(r2)]
            }
            AddrMode::RegCommaParRegEnsPlus(r1, r2) => {
                vec![self.reg_arg(r1), self.par_reg_ens_plus_arg(r2)]
            }
            AddrMode::RegCommaPoundImm8(r1)
            | AddrMode::RegCommaPoundImm16(r1) => {
                vec![self.reg_arg(r1), self.pound_imm_arg()]
            }
            AddrMode::RegCommaReg(r1, r2) => {
                vec![self.reg_arg(r1), self.reg_arg(r2)]
            }
        };
        let body = match addr_mode {
            AddrMode::Addr8
            | AddrMode::Addr8PlusReg(_)
            | AddrMode::Addr8PlusRegCommaReg(_, _)
            | AddrMode::Addr8CommaReg(_)
            | AddrMode::BracAddr8Kets
            | AddrMode::BracAddr8KetsCommaReg(_)
            | AddrMode::BracAddr8KetsPlusRegCommaReg(_, _)
            | AddrMode::BracAddr8PlusRegKetsCommaReg(_, _)
            | AddrMode::ParAddr8CommaRegEns(_)
            | AddrMode::ParAddr8CommaRegEnsCommaReg(_, _)
            | AddrMode::ParAddr8Ens
            | AddrMode::ParAddr8EnsCommaReg(_)
            | AddrMode::RegCommaAddr8(_)
            | AddrMode::RegCommaAddr8PlusReg(_, _)
            | AddrMode::RegCommaBracAddr8PlusRegKets(_, _)
            | AddrMode::RegCommaBracAddr8KetsPlusReg(_, _) => vec![
                constant_u8(opcode_byte),
                placeholder_u8(&self.placeholder_addr),
            ],
            AddrMode::Addr8CommaAddr8 => vec![
                constant_u8(opcode_byte),
                placeholder_u8(&self.placeholder_addr2),
                placeholder_u8(&self.placeholder_addr),
            ],
            AddrMode::Addr8CommaPoundImm8 => vec![
                constant_u8(opcode_byte),
                placeholder_u8(&self.placeholder_imm),
                placeholder_u8(&self.placeholder_addr),
            ],
            AddrMode::Addr16
            | AddrMode::BangAddr16
            | AddrMode::BangAddr16PlusRegCommaReg(_, _)
            | AddrMode::BangAddr16CommaReg(_)
            | AddrMode::BracBangAddr16Kets
            | AddrMode::BracBangAddr16PlusRegKets(_)
            | AddrMode::ParBangAddr16CommaRegEns(_)
            | AddrMode::ParBangAddr16Ens
            | AddrMode::RegCommaBangAddr16(_)
            | AddrMode::RegCommaBangAddr16PlusReg(_, _) => vec![
                constant_u8(opcode_byte),
                placeholder_u16le(&self.placeholder_addr),
            ],
            AddrMode::AddrHi => vec![
                constant_u8(opcode_byte),
                high_page_addr(&self.placeholder_addr),
            ],
            AddrMode::BangBangAddr24 | AddrMode::BangBangAddr24CommaReg(_) => {
                vec![
                    constant_u8(opcode_byte),
                    placeholder_u24le(&self.placeholder_addr),
                ]
            }
            AddrMode::Branch => vec![
                constant_u8(opcode_byte),
                relative_addr(&self.placeholder_addr),
            ],
            AddrMode::Implied
            | AddrMode::ParRegEns(_)
            | AddrMode::ParRegEnsCommaParRegEns(_, _)
            | AddrMode::ParRegEnsPlusCommaReg(_, _)
            | AddrMode::ParRegEnsCommaReg(_, _)
            | AddrMode::Reg(_)
            | AddrMode::RegCommaReg(_, _)
            | AddrMode::RegCommaParRegEns(_, _)
            | AddrMode::RegCommaParRegEnsPlus(_, _) => {
                vec![constant_u8(opcode_byte)]
            }
            AddrMode::PoundImm8 | AddrMode::RegCommaPoundImm8(_) => vec![
                constant_u8(opcode_byte),
                placeholder_u8(&self.placeholder_imm),
            ],
            AddrMode::PoundImm8CommaPoundImm8 => vec![
                constant_u8(opcode_byte),
                placeholder_u8(&self.placeholder_imm2),
                placeholder_u8(&self.placeholder_imm),
            ],
            AddrMode::PoundPoundImm16 | AddrMode::RegCommaPoundImm16(_) => {
                vec![
                    constant_u8(opcode_byte),
                    placeholder_u16le(&self.placeholder_imm),
                ]
            }
            AddrMode::SuperFxLink => super_fx_link(&self.placeholder_imm),
        };
        let definition = AsmDefMacroAst { id: builtin_id(name), params, body };
        let reserved = self.arch_tree.reserved_names(arch);
        self.macros.define(arch, reserved, definition).unwrap();
    }

    fn addr_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![token(TokenValue::Placeholder(
                self.placeholder_addr.clone(),
            ))],
        }
    }

    fn addr2_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![token(TokenValue::Placeholder(
                self.placeholder_addr2.clone(),
            ))],
        }
    }

    fn addr_plus_reg_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::Plus),
                self.register_token(reg),
            ],
        }
    }

    fn bang_addr_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Bang),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
            ],
        }
    }

    fn bang_bang_addr_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Bang),
                token(TokenValue::Bang),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
            ],
        }
    }

    fn bang_addr_plus_reg_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Bang),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::Plus),
                self.register_token(reg),
            ],
        }
    }

    fn brac_addr_kets_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::BracketOpen),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::BracketClose),
            ],
        }
    }

    fn brac_addr_kets_plus_reg_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::BracketOpen),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::BracketClose),
                token(TokenValue::Plus),
                self.register_token(reg),
            ],
        }
    }

    fn brac_addr_plus_reg_kets_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::BracketOpen),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::Plus),
                self.register_token(reg),
                token(TokenValue::BracketClose),
            ],
        }
    }

    fn brac_bang_addr_kets_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::BracketOpen),
                token(TokenValue::Bang),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::BracketClose),
            ],
        }
    }

    fn brac_bang_addr_plus_reg_kets_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::BracketOpen),
                token(TokenValue::Bang),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::Plus),
                self.register_token(reg),
                token(TokenValue::BracketClose),
            ],
        }
    }

    fn par_addr_comma_reg_ens_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::ParenOpen),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::Comma),
                self.register_token(reg),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_addr_ens_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::ParenOpen),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_bang_addr_ens_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::ParenOpen),
                token(TokenValue::Bang),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_bang_addr_comma_reg_ens_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::ParenOpen),
                token(TokenValue::Bang),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
                token(TokenValue::Comma),
                self.register_token(reg),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_reg_ens_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::ParenOpen),
                self.register_token(reg),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_reg_ens_plus_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::ParenOpen),
                self.register_token(reg),
                token(TokenValue::ParenClose),
                token(TokenValue::Plus),
            ],
        }
    }

    fn pound_imm_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Pound),
                token(TokenValue::Placeholder(self.placeholder_imm.clone())),
            ],
        }
    }

    fn pound_imm2_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Pound),
                token(TokenValue::Placeholder(self.placeholder_imm2.clone())),
            ],
        }
    }

    fn pound_pound_imm_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Pound),
                token(TokenValue::Pound),
                token(TokenValue::Placeholder(self.placeholder_imm.clone())),
            ],
        }
    }

    fn reg_arg(&self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![self.register_token(reg)],
        }
    }

    fn register_token(&self, reg: Reg) -> Token {
        let name = match reg {
            Reg::A => &self.reg_a,
            Reg::B => &self.reg_b,
            Reg::C => &self.reg_c,
            Reg::D => &self.reg_d,
            Reg::E => &self.reg_e,
            Reg::H => &self.reg_h,
            Reg::L => &self.reg_l,
            Reg::Psw => &self.reg_psw,
            Reg::R0 => &self.reg_r0,
            Reg::R1 => &self.reg_r1,
            Reg::R2 => &self.reg_r2,
            Reg::R3 => &self.reg_r3,
            Reg::R4 => &self.reg_r4,
            Reg::R5 => &self.reg_r5,
            Reg::R6 => &self.reg_r6,
            Reg::R7 => &self.reg_r7,
            Reg::R8 => &self.reg_r8,
            Reg::R9 => &self.reg_r9,
            Reg::R10 => &self.reg_r10,
            Reg::R11 => &self.reg_r11,
            Reg::R12 => &self.reg_r12,
            Reg::R13 => &self.reg_r13,
            Reg::R14 => &self.reg_r14,
            Reg::R15 => &self.reg_r15,
            Reg::S => &self.reg_s,
            Reg::Sp => &self.reg_sp,
            Reg::X => &self.reg_x,
            Reg::Y => &self.reg_y,
            Reg::Ya => &self.reg_ya,
        };
        token(TokenValue::Identifier(name.clone()))
    }
}

//===========================================================================//

fn builtin_id(name: &str) -> IdentifierAst {
    IdentifierAst {
        span: SrcSpan::BUILTIN,
        name: Rc::from(name),
        is_placeholder: false,
    }
}

fn binop_expr(op: BinOpAst, lhs: ExprAst, rhs: ExprAst) -> ExprAst {
    ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::BinOp(
            (SrcSpan::BUILTIN, op),
            Box::new(lhs),
            Box::new(rhs),
        ),
    }
}

fn constant_expr(value: u8) -> ExprAst {
    ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::IntLiteral(BigInt::from(value)),
    }
}

fn constant_u8(value: u8) -> AsmStmtAst {
    int_data_stmt(AsmIntTypeAst::U8, constant_expr(value))
}

fn high_page_addr(placeholder: &Rc<str>) -> AsmStmtAst {
    // TODO: add assert that high byte of address is valid
    let expr = binop_expr(
        BinOpAst::BitAnd,
        placeholder_expr(placeholder),
        constant_expr(0xff),
    );
    int_data_stmt(AsmIntTypeAst::U8, expr)
}

fn relative_addr(placeholder: &Rc<str>) -> AsmStmtAst {
    let here =
        ExprAst { span: SrcSpan::BUILTIN, node: ExprAstNode::HereLabel };
    let relative = binop_expr(
        BinOpAst::Sub,
        placeholder_expr(placeholder),
        binop_expr(BinOpAst::Add, here, constant_expr(1)),
    );
    // TODO: use .S8 instead of .U8 & $ff
    let expr = binop_expr(BinOpAst::BitAnd, relative, constant_expr(0xff));
    int_data_stmt(AsmIntTypeAst::U8, expr)
}

fn placeholder_u8(placeholder: &Rc<str>) -> AsmStmtAst {
    int_data_stmt(AsmIntTypeAst::U8, placeholder_expr(placeholder))
}

fn placeholder_u16le(placeholder: &Rc<str>) -> AsmStmtAst {
    int_data_stmt(AsmIntTypeAst::U16le, placeholder_expr(placeholder))
}

fn placeholder_u24le(placeholder: &Rc<str>) -> AsmStmtAst {
    int_data_stmt(AsmIntTypeAst::U24le, placeholder_expr(placeholder))
}

fn placeholder_expr(placeholder: &Rc<str>) -> ExprAst {
    ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::Placeholder(placeholder.clone()),
    }
}

fn super_fx_link(placeholder: &Rc<str>) -> Vec<AsmStmtAst> {
    // TODO: use a let statement to only eval the placeholder expression once
    // TODO: use logical AND
    let condition_expr = binop_expr(
        BinOpAst::BitAnd,
        binop_expr(
            BinOpAst::CmpGe,
            placeholder_expr(placeholder),
            constant_expr(1),
        ),
        binop_expr(
            BinOpAst::CmpLe,
            placeholder_expr(placeholder),
            constant_expr(4),
        ),
    );
    let message_expr = ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::StrLiteral(Rc::from(
            "LINK immediate value must be in the range [1, 4]",
        )),
    };
    let assert_stmt = AsmStmtAst::Assert(AsmAssertAst {
        condition: condition_expr,
        message: Some(message_expr),
    });
    let opcode_expr = binop_expr(
        BinOpAst::Add,
        constant_expr(0x90),
        placeholder_expr(placeholder),
    );
    let opcode_stmt = int_data_stmt(AsmIntTypeAst::U8, opcode_expr);
    vec![assert_stmt, opcode_stmt]
}

fn int_data_stmt(int_type: AsmIntTypeAst, expr: ExprAst) -> AsmStmtAst {
    AsmStmtAst::IntData(AsmIntDataAst {
        directive_span: SrcSpan::BUILTIN,
        int_type,
        expressions: vec![expr],
    })
}

fn token(value: TokenValue) -> Token {
    Token { span: SrcSpan::BUILTIN, value }
}

//===========================================================================//
