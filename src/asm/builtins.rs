use super::arch::ArchTree;
use super::macros::MacroTable;
use crate::error::SrcSpan;
use crate::parse::{
    AsmDefMacroAst, AsmMacroArgAst, AsmStmtAst, BinOpAst, ExprAst,
    ExprAstNode, IdentifierAst, Token, TokenValue,
};
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

const MACROS_65XX: &[(&str, u8, AddrMode)] = &[
    ("ASL", 0x0a, AddrMode::Reg(Reg::A)),
    ("BCC", 0x90, AddrMode::Branch),
    ("BCS", 0xb0, AddrMode::Branch),
    ("BEQ", 0xf0, AddrMode::Branch),
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
    ("DEX", 0xca, AddrMode::Implied),
    ("DEY", 0x88, AddrMode::Implied),
    ("INX", 0xe8, AddrMode::Implied),
    ("INY", 0xc8, AddrMode::Implied),
    ("JMP", 0x4c, AddrMode::BangAddr16),
    ("JSR", 0x20, AddrMode::BangAddr16),
    ("LSR", 0x4a, AddrMode::Reg(Reg::A)),
    ("NOP", 0xea, AddrMode::Implied),
    ("PHA", 0x48, AddrMode::Implied),
    ("PHP", 0x08, AddrMode::Implied),
    ("PLA", 0x68, AddrMode::Implied),
    ("PLP", 0x28, AddrMode::Implied),
    ("ROL", 0x2a, AddrMode::Reg(Reg::A)),
    ("ROR", 0x6a, AddrMode::Reg(Reg::A)),
    ("RTI", 0x40, AddrMode::Implied),
    ("RTS", 0x60, AddrMode::Implied),
    ("SEC", 0x38, AddrMode::Implied),
    ("SED", 0xf8, AddrMode::Implied),
    ("SEI", 0x78, AddrMode::Implied),
    ("STA", 0x8d, AddrMode::BangAddr16),
    ("STX", 0x8e, AddrMode::BangAddr16),
    ("STY", 0x8c, AddrMode::BangAddr16),
    ("TAX", 0xaa, AddrMode::Implied),
    ("TAY", 0xa8, AddrMode::Implied),
    ("TSX", 0xba, AddrMode::Implied),
    ("TXA", 0x8a, AddrMode::Implied),
    ("TXS", 0x9a, AddrMode::Implied),
    ("TYA", 0x98, AddrMode::Implied),
];

const MACROS_6502: &[(&str, u8, AddrMode)] = &[
    ("ADC", 0x69, AddrMode::PoundImm8),
    ("AND", 0x29, AddrMode::PoundImm8),
    ("CMP", 0xc9, AddrMode::PoundImm8),
    ("CPX", 0xe0, AddrMode::PoundImm8),
    ("CPY", 0xc0, AddrMode::PoundImm8),
    ("EOR", 0x49, AddrMode::PoundImm8),
    ("LDA", 0xa9, AddrMode::PoundImm8),
    ("LDX", 0xa2, AddrMode::PoundImm8),
    ("LDY", 0xa0, AddrMode::PoundImm8),
    ("ORA", 0x09, AddrMode::PoundImm8),
    ("SBC", 0xe9, AddrMode::PoundImm8),
];

const MACROS_65C816: &[(&str, u8, AddrMode)] = &[
    ("BRA", 0x80, AddrMode::Branch),
    ("COP", 0x02, AddrMode::PoundImm8),
    ("DEC", 0x3a, AddrMode::Reg(Reg::A)),
    ("INC", 0x1a, AddrMode::Reg(Reg::A)),
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
    ("STP", 0xdb, AddrMode::Implied),
    ("STZ", 0x9c, AddrMode::BangAddr16),
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
    /// FOO [addr] + R1, R2
    BracAddr8KetsPlusRegCommaReg(Reg, Reg),
    /// FOO [addr + R1], R2
    BracAddr8PlusRegKetsCommaReg(Reg, Reg),
    /// FOO [!addr + R]
    BracBangAddr16PlusRegKets(Reg),
    /// FOO addr
    Branch,
    /// FOO (R1), (R2)
    ParRegEnsCommaParRegEns(Reg, Reg),
    /// FOO (R1), R2
    ParRegEnsCommaReg(Reg, Reg),
    /// FOO (R1)+, R2
    ParRegEnsPlusCommaReg(Reg, Reg),
    /// FOO #imm
    PoundImm8,
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
    /// FOO R1, R2
    RegCommaReg(Reg, Reg),
}

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

    arch_tree
        .define_arch(arch_65xx.clone(), ArchTree::ROOT_ARCH_NAME, &res_65xx)
        .unwrap();
    arch_tree.define_arch(arch_6502.clone(), &arch_65xx, &[]).unwrap();
    arch_tree
        .define_arch(arch_65c816.clone(), &arch_65xx, &res_65c816)
        .unwrap();
    arch_tree
        .define_arch(arch_sm83.clone(), ArchTree::ROOT_ARCH_NAME, &res_sm83)
        .unwrap();
    arch_tree
        .define_arch(
            arch_spc700.clone(),
            ArchTree::ROOT_ARCH_NAME,
            &res_spc700,
        )
        .unwrap();

    let mut builder = BuiltinBuilder {
        arch_tree,
        macros: MacroTable::new(),
        placeholder_addr: Rc::from("%addr"),
        placeholder_addr2: Rc::from("%addr2"),
        placeholder_imm: Rc::from("%imm"),
        reg_a,
        reg_b,
        reg_c,
        reg_d,
        reg_e,
        reg_h,
        reg_l,
        reg_psw,
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
    (builder.arch_tree, builder.macros)
}

//===========================================================================//

struct BuiltinBuilder {
    arch_tree: ArchTree,
    macros: MacroTable,
    placeholder_addr: Rc<str>,
    placeholder_addr2: Rc<str>,
    placeholder_imm: Rc<str>,
    reg_a: Rc<str>,
    reg_b: Rc<str>,
    reg_c: Rc<str>,
    reg_d: Rc<str>,
    reg_e: Rc<str>,
    reg_h: Rc<str>,
    reg_l: Rc<str>,
    reg_psw: Rc<str>,
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
            AddrMode::BracAddr8KetsPlusRegCommaReg(r1, r2) => {
                vec![self.brac_addr_kets_plus_reg_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::BracAddr8PlusRegKetsCommaReg(r1, r2) => {
                vec![self.brac_addr_plus_reg_kets_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::BracBangAddr16PlusRegKets(r1) => {
                vec![self.brac_bang_addr_plus_reg_kets_arg(r1)]
            }
            AddrMode::Implied => vec![],
            AddrMode::ParRegEnsCommaParRegEns(r1, r2) => {
                vec![self.par_reg_ens_arg(r1), self.par_reg_ens_arg(r2)]
            }
            AddrMode::ParRegEnsPlusCommaReg(r1, r2) => {
                vec![self.par_reg_ens_plus_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::ParRegEnsCommaReg(r1, r2) => {
                vec![self.par_reg_ens_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::PoundImm8 => vec![self.pound_imm_arg()],
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
            AddrMode::RegCommaPoundImm8(r1) => {
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
            | AddrMode::BracAddr8KetsPlusRegCommaReg(_, _)
            | AddrMode::BracAddr8PlusRegKetsCommaReg(_, _)
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
            | AddrMode::BracBangAddr16PlusRegKets(_)
            | AddrMode::RegCommaBangAddr16(_)
            | AddrMode::RegCommaBangAddr16PlusReg(_, _) => vec![
                constant_u8(opcode_byte),
                placeholder_u16le(&self.placeholder_addr),
            ],
            AddrMode::AddrHi => vec![
                constant_u8(opcode_byte),
                high_page_addr(&self.placeholder_addr),
            ],
            AddrMode::Branch => vec![
                constant_u8(opcode_byte),
                relative_addr(&self.placeholder_addr),
            ],
            AddrMode::Implied
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

fn constant_expr(value: u8) -> ExprAst {
    ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::IntLiteral(BigInt::from(value)),
    }
}

fn constant_u8(value: u8) -> AsmStmtAst {
    AsmStmtAst::U8(constant_expr(value))
}

fn high_page_addr(placeholder: &Rc<str>) -> AsmStmtAst {
    // TODO: add assert that high byte of address is valid
    let expr = ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::BinOp(
            (SrcSpan::BUILTIN, BinOpAst::BitAnd),
            Box::new(placeholder_expr(placeholder)),
            Box::new(constant_expr(0xff)),
        ),
    };
    AsmStmtAst::U8(expr)
}

fn relative_addr(placeholder: &Rc<str>) -> AsmStmtAst {
    let here =
        ExprAst { span: SrcSpan::BUILTIN, node: ExprAstNode::HereLabel };
    let next = ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::BinOp(
            (SrcSpan::BUILTIN, BinOpAst::Add),
            Box::new(here),
            Box::new(constant_expr(1)),
        ),
    };
    let relative = ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::BinOp(
            (SrcSpan::BUILTIN, BinOpAst::Sub),
            Box::new(placeholder_expr(placeholder)),
            Box::new(next),
        ),
    };
    // TODO: use .S8 instead of .U8 & $ff
    let expr = ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::BinOp(
            (SrcSpan::BUILTIN, BinOpAst::BitAnd),
            Box::new(relative),
            Box::new(constant_expr(0xff)),
        ),
    };
    AsmStmtAst::U8(expr)
}

fn placeholder_u8(placeholder: &Rc<str>) -> AsmStmtAst {
    AsmStmtAst::U8(placeholder_expr(placeholder))
}

fn placeholder_u16le(placeholder: &Rc<str>) -> AsmStmtAst {
    AsmStmtAst::U16le(placeholder_expr(placeholder))
}

fn placeholder_expr(placeholder: &Rc<str>) -> ExprAst {
    ExprAst {
        span: SrcSpan::BUILTIN,
        node: ExprAstNode::Placeholder(placeholder.clone()),
    }
}

fn token(value: TokenValue) -> Token {
    Token { span: SrcSpan::BUILTIN, value }
}

//===========================================================================//
