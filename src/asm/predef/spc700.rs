use super::addrmode::{
    AddrMode, PLACEHOLDER_ADDR, PLACEHOLDER_ADDR2, PLACEHOLDER_IMM, Reg,
    addr_arg, bang_addr_arg, macro_arg, par_reg_ens_arg, pound_imm_arg,
    reg_arg, token,
};
use super::pool::RcPool;
use crate::lex::TokenValue;
use crate::parse::{
    AsmIntTypeAst, AsmMacroArgAst, AsmStmtAst, BinOpAst, ExprAst,
};

//===========================================================================//

pub(super) const ARCH_SPC700: &str = "SPC700";
pub(super) const RES_SPC700: &[&str] =
    &["A", "C", "PSW", "SP", "X", "Y", "YA"];
pub(super) const MACROS_SPC700: &[(&str, &[u8], Spc700)] = &[
    ("ADC", &[0x84], Spc700::RegCommaAddr8("A")),
    ("ADC", &[0x85], Spc700::RegCommaBangAddr16("A")),
    ("ADC", &[0x86], Spc700::RegCommaParRegEns("A", "X")),
    ("ADC", &[0x87], Spc700::RegCommaBracAddr8PlusRegKets("A", "X")),
    ("ADC", &[0x88], Spc700::RegCommaPoundImm8("A")),
    ("ADC", &[0x89], Spc700::Addr8CommaAddr8),
    ("ADC", &[0x94], Spc700::RegCommaAddr8PlusReg("A", "X")),
    ("ADC", &[0x95], Spc700::RegCommaBangAddr16PlusReg("A", "X")),
    ("ADC", &[0x96], Spc700::RegCommaBangAddr16PlusReg("A", "Y")),
    ("ADC", &[0x97], Spc700::RegCommaBracAddr8KetsPlusReg("A", "Y")),
    ("ADC", &[0x98], Spc700::Addr8CommaPoundImm8),
    ("ADC", &[0x99], Spc700::ParRegEnsCommaParRegEns("X", "Y")),
    ("ADDW", &[0x7a], Spc700::RegCommaAddr8("YA")),
    ("AND", &[0x24], Spc700::RegCommaAddr8("A")),
    ("AND", &[0x25], Spc700::RegCommaBangAddr16("A")),
    ("AND", &[0x26], Spc700::RegCommaParRegEns("A", "X")),
    ("AND", &[0x27], Spc700::RegCommaBracAddr8PlusRegKets("A", "X")),
    ("AND", &[0x28], Spc700::RegCommaPoundImm8("A")),
    ("AND", &[0x29], Spc700::Addr8CommaAddr8),
    ("AND", &[0x34], Spc700::RegCommaAddr8PlusReg("A", "X")),
    ("AND", &[0x35], Spc700::RegCommaBangAddr16PlusReg("A", "X")),
    ("AND", &[0x36], Spc700::RegCommaBangAddr16PlusReg("A", "Y")),
    ("AND", &[0x37], Spc700::RegCommaBracAddr8KetsPlusReg("A", "Y")),
    ("AND", &[0x38], Spc700::Addr8CommaPoundImm8),
    ("AND", &[0x39], Spc700::ParRegEnsCommaParRegEns("X", "Y")),
    ("AND1", &[0x4a], Spc700::RegCommaAddr13CommaBit("C")),
    ("AND1", &[0x6a], Spc700::RegCommaSlashAddr13CommaBit("C")),
    ("ASL", &[0x0b], Spc700::Addr8),
    ("ASL", &[0x0c], Spc700::BangAddr16),
    ("ASL", &[0x1b], Spc700::Addr8PlusReg("X")),
    ("ASL", &[0x1c], Spc700::Reg("A")),
    ("BBC", &[0x13], Spc700::Addr8CommaBitCommaRelative8),
    ("BBS", &[0x03], Spc700::Addr8CommaBitCommaRelative8),
    ("BCC", &[0x90], Spc700::Relative8),
    ("BCS", &[0xb0], Spc700::Relative8),
    ("BEQ", &[0xf0], Spc700::Relative8),
    ("BMI", &[0x30], Spc700::Relative8),
    ("BNE", &[0xd0], Spc700::Relative8),
    ("BPL", &[0x10], Spc700::Relative8),
    ("BRA", &[0x2f], Spc700::Relative8),
    ("BRK", &[0x0f], Spc700::Implied),
    ("BVC", &[0x50], Spc700::Relative8),
    ("BVS", &[0x70], Spc700::Relative8),
    ("CALL", &[0x3f], Spc700::BangAddr16),
    ("CBNE", &[0x2e], Spc700::Addr8CommaRelative8),
    ("CBNE", &[0xde], Spc700::Addr8PlusRegCommaRelative8("X")),
    ("CLR1", &[0x12], Spc700::Addr8CommaBit),
    ("CLRC", &[0x60], Spc700::Implied),
    ("CLRP", &[0x20], Spc700::Implied),
    ("CLRV", &[0xe0], Spc700::Implied),
    ("CMP", &[0x1e], Spc700::RegCommaBangAddr16("X")),
    ("CMP", &[0x3e], Spc700::RegCommaAddr8("X")),
    ("CMP", &[0x5e], Spc700::RegCommaBangAddr16("Y")),
    ("CMP", &[0x64], Spc700::RegCommaAddr8("A")),
    ("CMP", &[0x65], Spc700::RegCommaBangAddr16("A")),
    ("CMP", &[0x66], Spc700::RegCommaParRegEns("A", "X")),
    ("CMP", &[0x67], Spc700::RegCommaBracAddr8PlusRegKets("A", "X")),
    ("CMP", &[0x68], Spc700::RegCommaPoundImm8("A")),
    ("CMP", &[0x69], Spc700::Addr8CommaAddr8),
    ("CMP", &[0x74], Spc700::RegCommaAddr8PlusReg("A", "X")),
    ("CMP", &[0x75], Spc700::RegCommaBangAddr16PlusReg("A", "X")),
    ("CMP", &[0x76], Spc700::RegCommaBangAddr16PlusReg("A", "Y")),
    ("CMP", &[0x77], Spc700::RegCommaBracAddr8KetsPlusReg("A", "Y")),
    ("CMP", &[0x78], Spc700::Addr8CommaPoundImm8),
    ("CMP", &[0x79], Spc700::ParRegEnsCommaParRegEns("X", "Y")),
    ("CMP", &[0x7e], Spc700::RegCommaAddr8("Y")),
    ("CMP", &[0xad], Spc700::RegCommaPoundImm8("Y")),
    ("CMP", &[0xc8], Spc700::RegCommaPoundImm8("X")),
    ("CMPW", &[0x5a], Spc700::RegCommaAddr8("YA")),
    ("DAA", &[0xdf], Spc700::Reg("A")),
    ("DAS", &[0xbe], Spc700::Reg("A")),
    ("DBNZ", &[0x6e], Spc700::Addr8CommaRelative8),
    ("DBNZ", &[0xfe], Spc700::RegCommaRelative8("Y")),
    ("DEC", &[0x8b], Spc700::Addr8),
    ("DEC", &[0x1d], Spc700::Reg("X")),
    ("DEC", &[0x8c], Spc700::BangAddr16),
    ("DEC", &[0x8c], Spc700::BangAddr16),
    ("DEC", &[0x9b], Spc700::Addr8PlusReg("X")),
    ("DEC", &[0x9c], Spc700::Reg("A")),
    ("DEC", &[0xdc], Spc700::Reg("Y")),
    ("DECW", &[0x1a], Spc700::Addr8),
    ("DI", &[0xc0], Spc700::Implied),
    ("DIV", &[0x9e], Spc700::RegCommaReg("YA", "X")),
    ("EI", &[0xa0], Spc700::Implied),
    ("EOR", &[0x44], Spc700::RegCommaAddr8("A")),
    ("EOR", &[0x45], Spc700::RegCommaBangAddr16("A")),
    ("EOR", &[0x46], Spc700::RegCommaParRegEns("A", "X")),
    ("EOR", &[0x47], Spc700::RegCommaBracAddr8PlusRegKets("A", "X")),
    ("EOR", &[0x48], Spc700::RegCommaPoundImm8("A")),
    ("EOR", &[0x49], Spc700::Addr8CommaAddr8),
    ("EOR", &[0x54], Spc700::RegCommaAddr8PlusReg("A", "X")),
    ("EOR", &[0x55], Spc700::RegCommaBangAddr16PlusReg("A", "X")),
    ("EOR", &[0x56], Spc700::RegCommaBangAddr16PlusReg("A", "Y")),
    ("EOR", &[0x57], Spc700::RegCommaBracAddr8KetsPlusReg("A", "Y")),
    ("EOR", &[0x58], Spc700::Addr8CommaPoundImm8),
    ("EOR", &[0x59], Spc700::ParRegEnsCommaParRegEns("X", "Y")),
    ("EOR1", &[0x8a], Spc700::RegCommaAddr13CommaBit("C")),
    ("INC", &[0xab], Spc700::Addr8),
    ("INC", &[0x3d], Spc700::Reg("X")),
    ("INC", &[0xac], Spc700::BangAddr16),
    ("INC", &[0xbb], Spc700::Addr8PlusReg("X")),
    ("INC", &[0xbc], Spc700::Reg("A")),
    ("INC", &[0xfc], Spc700::Reg("Y")),
    ("INCW", &[0x3a], Spc700::Addr8),
    ("JMP", &[0x1f], Spc700::BracBangAddr16PlusRegKets("X")),
    ("JMP", &[0x5f], Spc700::BangAddr16),
    ("LSR", &[0x4b], Spc700::Addr8),
    ("LSR", &[0x4c], Spc700::BangAddr16),
    ("LSR", &[0x5b], Spc700::Addr8PlusReg("X")),
    ("LSR", &[0x5c], Spc700::Reg("A")),
    ("MOV", &[0xfa], Spc700::Addr8CommaAddr8),
    ("MOV", &[0x8f], Spc700::Addr8CommaPoundImm8),
    ("MOV", &[0xf8], Spc700::RegCommaAddr8("X")),
    ("MOV", &[0xeb], Spc700::RegCommaAddr8("Y")),
    ("MOV", &[0x8d], Spc700::RegCommaPoundImm8("Y")),
    ("MOV", &[0xcd], Spc700::RegCommaPoundImm8("X")),
    ("MOV", &[0xe4], Spc700::RegCommaAddr8("A")),
    ("MOV", &[0xe5], Spc700::RegCommaBangAddr16("A")),
    ("MOV", &[0xe6], Spc700::RegCommaParRegEns("A", "X")),
    ("MOV", &[0xe7], Spc700::RegCommaBracAddr8PlusRegKets("A", "X")),
    ("MOV", &[0xe8], Spc700::RegCommaPoundImm8("A")),
    ("MOV", &[0xe9], Spc700::RegCommaBangAddr16("X")),
    ("MOV", &[0xec], Spc700::RegCommaBangAddr16("Y")),
    ("MOV", &[0xf4], Spc700::RegCommaAddr8PlusReg("A", "X")),
    ("MOV", &[0xf5], Spc700::RegCommaBangAddr16PlusReg("A", "X")),
    ("MOV", &[0xf6], Spc700::RegCommaBangAddr16PlusReg("A", "Y")),
    ("MOV", &[0xf7], Spc700::RegCommaBracAddr8KetsPlusReg("A", "Y")),
    ("MOV", &[0xf9], Spc700::RegCommaAddr8PlusReg("X", "Y")),
    ("MOV", &[0xfb], Spc700::RegCommaAddr8PlusReg("Y", "X")),
    ("MOV", &[0xbf], Spc700::RegCommaParRegEnsPlus("A", "X")),
    ("MOV", &[0xc4], Spc700::Addr8CommaReg("A")),
    ("MOV", &[0xd4], Spc700::Addr8PlusRegCommaReg("X", "A")),
    ("MOV", &[0xc5], Spc700::BangAddr16CommaReg("A")),
    ("MOV", &[0xd5], Spc700::BangAddr16PlusRegCommaReg("X", "A")),
    ("MOV", &[0xd6], Spc700::BangAddr16PlusRegCommaReg("Y", "A")),
    ("MOV", &[0xc7], Spc700::BracAddr8PlusRegKetsCommaReg("X", "A")),
    ("MOV", &[0xd7], Spc700::BracAddr8KetsPlusRegCommaReg("Y", "A")),
    ("MOV", &[0xd8], Spc700::Addr8CommaReg("X")),
    ("MOV", &[0xd9], Spc700::Addr8PlusRegCommaReg("Y", "X")),
    ("MOV", &[0xc9], Spc700::BangAddr16CommaReg("X")),
    ("MOV", &[0xcb], Spc700::Addr8CommaReg("Y")),
    ("MOV", &[0xdb], Spc700::Addr8PlusRegCommaReg("X", "Y")),
    ("MOV", &[0xcc], Spc700::BangAddr16CommaReg("Y")),
    ("MOV", &[0xc6], Spc700::ParRegEnsCommaReg("X", "A")),
    ("MOV", &[0xaf], Spc700::ParRegEnsPlusCommaReg("X", "A")),
    ("MOV", &[0x5d], Spc700::RegCommaReg("X", "A")),
    ("MOV", &[0x7d], Spc700::RegCommaReg("A", "X")),
    ("MOV", &[0xfd], Spc700::RegCommaReg("Y", "A")),
    ("MOV", &[0x9d], Spc700::RegCommaReg("X", "SP")),
    ("MOV", &[0xbd], Spc700::RegCommaReg("SP", "X")),
    ("MOV", &[0xdd], Spc700::RegCommaReg("A", "Y")),
    ("MOV1", &[0xaa], Spc700::RegCommaAddr13CommaBit("C")),
    ("MOV1", &[0xca], Spc700::Addr13CommaBitCommaReg("C")),
    ("MOVW", &[0xba], Spc700::RegCommaAddr8("YA")),
    ("MOVW", &[0xda], Spc700::Addr8CommaReg("YA")),
    ("MUL", &[0xcf], Spc700::Reg("YA")),
    ("NOP", &[0x00], Spc700::Implied),
    ("NOT1", &[0xea], Spc700::Addr13CommaBit),
    ("NOTC", &[0xed], Spc700::Implied),
    ("OR", &[0x04], Spc700::RegCommaAddr8("A")),
    ("OR", &[0x05], Spc700::RegCommaBangAddr16("A")),
    ("OR", &[0x06], Spc700::RegCommaParRegEns("A", "X")),
    ("OR", &[0x07], Spc700::RegCommaBracAddr8PlusRegKets("A", "X")),
    ("OR", &[0x08], Spc700::RegCommaPoundImm8("A")),
    ("OR", &[0x09], Spc700::Addr8CommaAddr8),
    ("OR", &[0x14], Spc700::RegCommaAddr8PlusReg("A", "X")),
    ("OR", &[0x15], Spc700::RegCommaBangAddr16PlusReg("A", "X")),
    ("OR", &[0x16], Spc700::RegCommaBangAddr16PlusReg("A", "Y")),
    ("OR", &[0x17], Spc700::RegCommaBracAddr8KetsPlusReg("A", "Y")),
    ("OR", &[0x18], Spc700::Addr8CommaPoundImm8),
    ("OR", &[0x19], Spc700::ParRegEnsCommaParRegEns("X", "Y")),
    ("OR1", &[0x0a], Spc700::RegCommaAddr13CommaBit("C")),
    ("OR1", &[0x2a], Spc700::RegCommaSlashAddr13CommaBit("C")),
    ("PCALL", &[0x4f], Spc700::AddrHi),
    ("POP", &[0x8e], Spc700::Reg("PSW")),
    ("POP", &[0xae], Spc700::Reg("A")),
    ("POP", &[0xce], Spc700::Reg("X")),
    ("POP", &[0xee], Spc700::Reg("Y")),
    ("PUSH", &[0x0d], Spc700::Reg("PSW")),
    ("PUSH", &[0x2d], Spc700::Reg("A")),
    ("PUSH", &[0x4d], Spc700::Reg("X")),
    ("PUSH", &[0x6d], Spc700::Reg("Y")),
    ("RET", &[0x6f], Spc700::Implied),
    ("RETI", &[0x7f], Spc700::Implied),
    ("ROL", &[0x2b], Spc700::Addr8),
    ("ROL", &[0x2c], Spc700::BangAddr16),
    ("ROL", &[0x3b], Spc700::Addr8PlusReg("X")),
    ("ROL", &[0x3c], Spc700::Reg("A")),
    ("ROR", &[0x6b], Spc700::Addr8),
    ("ROR", &[0x6c], Spc700::BangAddr16),
    ("ROR", &[0x7b], Spc700::Addr8PlusReg("X")),
    ("ROR", &[0x7c], Spc700::Reg("A")),
    ("SBC", &[0xa4], Spc700::RegCommaAddr8("A")),
    ("SBC", &[0xa5], Spc700::RegCommaBangAddr16("A")),
    ("SBC", &[0xa6], Spc700::RegCommaParRegEns("A", "X")),
    ("SBC", &[0xa7], Spc700::RegCommaBracAddr8PlusRegKets("A", "X")),
    ("SBC", &[0xa8], Spc700::RegCommaPoundImm8("A")),
    ("SBC", &[0xa9], Spc700::Addr8CommaAddr8),
    ("SBC", &[0xb4], Spc700::RegCommaAddr8PlusReg("A", "X")),
    ("SBC", &[0xb5], Spc700::RegCommaBangAddr16PlusReg("A", "X")),
    ("SBC", &[0xb6], Spc700::RegCommaBangAddr16PlusReg("A", "Y")),
    ("SBC", &[0xb7], Spc700::RegCommaBracAddr8KetsPlusReg("A", "Y")),
    ("SBC", &[0xb8], Spc700::Addr8CommaPoundImm8),
    ("SBC", &[0xb9], Spc700::ParRegEnsCommaParRegEns("X", "Y")),
    ("SET1", &[0x02], Spc700::Addr8CommaBit),
    ("SETC", &[0x80], Spc700::Implied),
    ("SETP", &[0x40], Spc700::Implied),
    ("SLEEP", &[0xef], Spc700::Implied),
    ("STOP", &[0xff], Spc700::Implied),
    ("SUBW", &[0x9a], Spc700::RegCommaAddr8("YA")),
    ("TCALL", &[0x01], Spc700::TcallIndex),
    ("TCLR1", &[0x4e], Spc700::BangAddr16),
    ("TSET1", &[0x0e], Spc700::BangAddr16),
    ("XCN", &[0x9f], Spc700::Reg("A")),
];

//===========================================================================//

#[derive(Clone, Copy)]
pub(super) enum Spc700 {
    /// FOO addr
    Addr8,
    /// FOO addr1, addr2
    Addr8CommaAddr8,
    /// FOO addr, bit
    Addr8CommaBit,
    /// FOO addr1, bit, addr2
    Addr8CommaBitCommaRelative8,
    /// FOO addr, #imm
    Addr8CommaPoundImm8,
    /// FOO addr, R
    Addr8CommaReg(Reg),
    /// FOO addr + R
    Addr8PlusReg(Reg),
    /// FOO addr1, addr2
    Addr8CommaRelative8,
    /// FOO addr + R1, R2
    Addr8PlusRegCommaReg(Reg, Reg),
    /// FOO addr1 + R, addr2
    Addr8PlusRegCommaRelative8(Reg),
    /// FOO addr, bit
    Addr13CommaBit,
    /// FOO addr, bit, R
    Addr13CommaBitCommaReg(Reg),
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
    /// FOO
    Implied,
    /// FOO (R1), (R2)
    ParRegEnsCommaParRegEns(Reg, Reg),
    /// FOO (R1), R2
    ParRegEnsCommaReg(Reg, Reg),
    /// FOO (R1)+, R2
    ParRegEnsPlusCommaReg(Reg, Reg),
    /// FOO reg
    Reg(Reg),
    /// FOO R, addr
    RegCommaAddr8(Reg),
    /// FOO R1, addr + R2
    RegCommaAddr8PlusReg(Reg, Reg),
    /// FOO R, addr, bit
    RegCommaAddr13CommaBit(Reg),
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
    /// FOO R, addr
    RegCommaRelative8(Reg),
    /// FOO R, /addr, bit
    RegCommaSlashAddr13CommaBit(Reg),
    /// FOO addr
    Relative8,
    /// TCALL index
    TcallIndex,
}

impl AddrMode for Spc700 {
    fn macro_args(&self, pool: &mut RcPool) -> Vec<AsmMacroArgAst> {
        match *self {
            Self::Addr8 | Self::AddrHi | Self::Relative8 => {
                vec![addr_arg(pool)]
            }
            Self::Addr8CommaAddr8 | Self::Addr8CommaRelative8 => {
                vec![addr_arg(pool), addr2_arg(pool)]
            }
            Self::Addr8CommaPoundImm8 => {
                vec![addr_arg(pool), pound_imm_arg(pool)]
            }
            Self::Addr8CommaReg(r1) => {
                vec![addr_arg(pool), reg_arg(pool, r1)]
            }
            Self::Addr8CommaBit | Self::Addr13CommaBit => {
                vec![addr_arg(pool), index_arg(pool)]
            }
            Self::Addr8CommaBitCommaRelative8 => {
                vec![addr_arg(pool), index_arg(pool), addr2_arg(pool)]
            }
            Self::Addr8PlusReg(r1) => {
                vec![addr_plus_reg_arg(pool, r1)]
            }
            Self::Addr8PlusRegCommaReg(r1, r2) => {
                vec![addr_plus_reg_arg(pool, r1), reg_arg(pool, r2)]
            }
            Self::Addr8PlusRegCommaRelative8(r1) => {
                vec![addr_plus_reg_arg(pool, r1), addr2_arg(pool)]
            }
            Self::Addr13CommaBitCommaReg(r1) => {
                vec![addr_arg(pool), index_arg(pool), reg_arg(pool, r1)]
            }
            Self::BangAddr16 => vec![bang_addr_arg(pool)],
            Self::BangAddr16PlusRegCommaReg(r1, r2) => {
                vec![bang_addr_plus_reg_arg(pool, r1), reg_arg(pool, r2)]
            }
            Self::BangAddr16CommaReg(r1) => {
                vec![bang_addr_arg(pool), reg_arg(pool, r1)]
            }
            Self::BracAddr8KetsPlusRegCommaReg(r1, r2) => {
                vec![brac_addr_kets_plus_reg_arg(pool, r1), reg_arg(pool, r2)]
            }
            Self::BracAddr8PlusRegKetsCommaReg(r1, r2) => {
                vec![brac_addr_plus_reg_kets_arg(pool, r1), reg_arg(pool, r2)]
            }
            Self::BracBangAddr16PlusRegKets(r1) => {
                vec![brac_bang_addr_plus_reg_kets_arg(pool, r1)]
            }
            Self::Implied => vec![],
            Self::ParRegEnsCommaParRegEns(r1, r2) => {
                vec![par_reg_ens_arg(pool, r1), par_reg_ens_arg(pool, r2)]
            }
            Self::ParRegEnsPlusCommaReg(r1, r2) => {
                vec![par_reg_ens_plus_arg(pool, r1), reg_arg(pool, r2)]
            }
            Self::ParRegEnsCommaReg(r1, r2) => {
                vec![par_reg_ens_arg(pool, r1), reg_arg(pool, r2)]
            }
            Self::Reg(r1) => vec![reg_arg(pool, r1)],
            Self::RegCommaAddr8(r1) | Self::RegCommaRelative8(r1) => {
                vec![reg_arg(pool, r1), addr_arg(pool)]
            }
            Self::RegCommaAddr8PlusReg(r1, r2) => {
                vec![reg_arg(pool, r1), addr_plus_reg_arg(pool, r2)]
            }
            Self::RegCommaAddr13CommaBit(r1) => {
                vec![reg_arg(pool, r1), addr_arg(pool), index_arg(pool)]
            }
            Self::RegCommaBangAddr16(r1) => {
                vec![reg_arg(pool, r1), bang_addr_arg(pool)]
            }
            Self::RegCommaBangAddr16PlusReg(r1, r2) => {
                vec![reg_arg(pool, r1), bang_addr_plus_reg_arg(pool, r2)]
            }
            Self::RegCommaBracAddr8KetsPlusReg(r1, r2) => {
                vec![reg_arg(pool, r1), brac_addr_kets_plus_reg_arg(pool, r2)]
            }
            Self::RegCommaBracAddr8PlusRegKets(r1, r2) => {
                vec![reg_arg(pool, r1), brac_addr_plus_reg_kets_arg(pool, r2)]
            }
            Self::RegCommaParRegEns(r1, r2) => {
                vec![reg_arg(pool, r1), par_reg_ens_arg(pool, r2)]
            }
            Self::RegCommaParRegEnsPlus(r1, r2) => {
                vec![reg_arg(pool, r1), par_reg_ens_plus_arg(pool, r2)]
            }
            Self::RegCommaPoundImm8(r1) => {
                vec![reg_arg(pool, r1), pound_imm_arg(pool)]
            }
            Self::RegCommaReg(r1, r2) => {
                vec![reg_arg(pool, r1), reg_arg(pool, r2)]
            }
            Self::RegCommaSlashAddr13CommaBit(r1) => {
                vec![reg_arg(pool, r1), slash_addr_arg(pool), index_arg(pool)]
            }
            Self::TcallIndex => vec![index_arg(pool)],
        }
    }

    fn macro_body(
        &self,
        pool: &mut RcPool,
        prefix_bytes: &[u8],
    ) -> Vec<AsmStmtAst> {
        match *self {
            Self::Addr8
            | Self::Addr8PlusReg(_)
            | Self::Addr8PlusRegCommaReg(_, _)
            | Self::Addr8CommaReg(_)
            | Self::BracAddr8KetsPlusRegCommaReg(_, _)
            | Self::BracAddr8PlusRegKetsCommaReg(_, _)
            | Self::RegCommaAddr8(_)
            | Self::RegCommaAddr8PlusReg(_, _)
            | Self::RegCommaBracAddr8PlusRegKets(_, _)
            | Self::RegCommaBracAddr8KetsPlusReg(_, _) => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            Self::Addr8CommaAddr8 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_ADDR2),
                pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            Self::Addr8CommaBit => vec![
                addr8_comma_bit_opcode(pool, prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            Self::Addr8CommaBitCommaRelative8 => vec![
                addr8_comma_bit_opcode(pool, prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_ADDR),
                pool.placeholder_addr16_rel8(PLACEHOLDER_ADDR2),
            ],
            Self::Addr8CommaRelative8
            | Self::Addr8PlusRegCommaRelative8(_) => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_ADDR),
                pool.placeholder_addr16_rel8(PLACEHOLDER_ADDR2),
            ],
            Self::Addr8CommaPoundImm8 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_IMM),
                pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            Self::BangAddr16
            | Self::BangAddr16PlusRegCommaReg(_, _)
            | Self::BangAddr16CommaReg(_)
            | Self::BracBangAddr16PlusRegKets(_)
            | Self::RegCommaBangAddr16(_)
            | Self::RegCommaBangAddr16PlusReg(_, _) => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u16le(PLACEHOLDER_ADDR),
            ],
            Self::AddrHi => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.high_page_addr(PLACEHOLDER_ADDR),
            ],
            Self::Implied
            | Self::ParRegEnsCommaParRegEns(_, _)
            | Self::ParRegEnsPlusCommaReg(_, _)
            | Self::ParRegEnsCommaReg(_, _)
            | Self::Reg(_)
            | Self::RegCommaReg(_, _)
            | Self::RegCommaParRegEns(_, _)
            | Self::RegCommaParRegEnsPlus(_, _) => {
                vec![pool.constant_bytes_stmt(prefix_bytes)]
            }
            Self::RegCommaPoundImm8(_) => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_IMM),
            ],
            Self::Addr13CommaBit
            | Self::Addr13CommaBitCommaReg(_)
            | Self::RegCommaAddr13CommaBit(_)
            | Self::RegCommaSlashAddr13CommaBit(_) => {
                vec![
                    pool.constant_bytes_stmt(prefix_bytes),
                    addr13_comma_bit_address(pool),
                ]
            }
            Self::Relative8 | Self::RegCommaRelative8(_) => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_addr16_rel8(PLACEHOLDER_ADDR),
            ],
            Self::TcallIndex => vec![tcall_index_opcode(pool, prefix_bytes)],
        }
    }
}

//===========================================================================//

fn addr2_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![pool.placeholder_token(PLACEHOLDER_ADDR2)])
}

fn addr_plus_reg_arg(pool: &mut RcPool, reg: Reg) -> AsmMacroArgAst {
    macro_arg(vec![
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::Plus),
        pool.identifier_token(reg),
    ])
}

fn bang_addr_plus_reg_arg(pool: &mut RcPool, reg: Reg) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::Bang),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::Plus),
        pool.identifier_token(reg),
    ])
}

fn brac_addr_kets_plus_reg_arg(pool: &mut RcPool, reg: Reg) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::BracketOpen),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::BracketClose),
        token(TokenValue::Plus),
        pool.identifier_token(reg),
    ])
}

fn brac_addr_plus_reg_kets_arg(pool: &mut RcPool, reg: Reg) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::BracketOpen),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::Plus),
        pool.identifier_token(reg),
        token(TokenValue::BracketClose),
    ])
}

fn brac_bang_addr_plus_reg_kets_arg(
    pool: &mut RcPool,
    reg: Reg,
) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::BracketOpen),
        token(TokenValue::Bang),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::Plus),
        pool.identifier_token(reg),
        token(TokenValue::BracketClose),
    ])
}

fn par_reg_ens_plus_arg(pool: &mut RcPool, reg: Reg) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::ParenOpen),
        pool.identifier_token(reg),
        token(TokenValue::ParenClose),
        token(TokenValue::Plus),
    ])
}

fn index_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![pool.placeholder_token(PLACEHOLDER_IMM)])
}

fn slash_addr_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::Slash),
        pool.placeholder_token(PLACEHOLDER_ADDR),
    ])
}

//===========================================================================//

fn addr8_comma_bit_opcode(
    pool: &mut RcPool,
    prefix_bytes: &[u8],
) -> AsmStmtAst {
    debug_assert_eq!(prefix_bytes.len(), 1);
    let opcode_expr = {
        let lhs = pool.int_literal_expr(i32::from(prefix_bytes[0]));
        let rhs = {
            let lhs = bit_expr(pool);
            let rhs = pool.int_literal_expr(5);
            pool.binop_expr(BinOpAst::Shl, lhs, rhs)
        };
        pool.binop_expr(BinOpAst::Add, lhs, rhs)
    };
    pool.int_data_stmt(AsmIntTypeAst::U8, opcode_expr)
}

fn addr13_comma_bit_address(pool: &mut RcPool) -> AsmStmtAst {
    let expr = {
        // TODO: error unless address in range [0x0000, 0x1fff]
        let lhs = pool.placeholder_expr(PLACEHOLDER_ADDR);
        let rhs = {
            let lhs = bit_expr(pool);
            let rhs = pool.int_literal_expr(13);
            pool.binop_expr(BinOpAst::Shl, lhs, rhs)
        };
        pool.binop_expr(BinOpAst::BitOr, lhs, rhs)
    };
    pool.int_data_stmt(AsmIntTypeAst::U16le, expr)
}

fn bit_expr(pool: &mut RcPool) -> ExprAst {
    // TODO: error unless the bit number is in the range [0, 7]
    pool.placeholder_expr(PLACEHOLDER_IMM)
}

fn tcall_index_opcode(pool: &mut RcPool, prefix_bytes: &[u8]) -> AsmStmtAst {
    debug_assert_eq!(prefix_bytes.len(), 1);
    let opcode_expr = {
        let lhs = pool.int_literal_expr(i32::from(prefix_bytes[0]));
        let rhs = {
            let lhs = {
                // TODO: error unless the index is in the range [0, 15]
                pool.placeholder_expr(PLACEHOLDER_IMM)
            };
            let rhs = pool.int_literal_expr(4);
            pool.binop_expr(BinOpAst::Shl, lhs, rhs)
        };
        pool.binop_expr(BinOpAst::Add, lhs, rhs)
    };
    pool.int_data_stmt(AsmIntTypeAst::U8, opcode_expr)
}

//===========================================================================//
