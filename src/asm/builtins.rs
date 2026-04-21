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
        placeholder_imm: Rc::from("%imm"),
        reg_a,
    };
    builder.add_65xx_macros(&arch_65xx);
    builder.add_6502_macros(&arch_6502);
    builder.add_65c816_macros(&arch_65c816);
    builder.add_sm83_macros(&arch_sm83);
    builder.add_spc700_macros(&arch_spc700);
    (builder.arch_tree, builder.macros)
}

//===========================================================================//

struct BuiltinBuilder {
    arch_tree: ArchTree,
    macros: MacroTable,
    placeholder_addr: Rc<str>,
    placeholder_imm: Rc<str>,
    reg_a: Rc<str>,
}

impl BuiltinBuilder {
    fn add_65xx_macros(&mut self, arch: &Rc<str>) {
        self.add_accum_opcode(arch, 0x0a, "ASL");
        self.add_addr16_opcode(arch, 0x2c, "BIT");
        self.add_branch_opcode(arch, 0x90, "BCC");
        self.add_branch_opcode(arch, 0xb0, "BCS");
        self.add_branch_opcode(arch, 0xf0, "BEQ");
        self.add_branch_opcode(arch, 0x30, "BMI");
        self.add_branch_opcode(arch, 0xd0, "BNE");
        self.add_branch_opcode(arch, 0x10, "BPL");
        self.add_branch_opcode(arch, 0x50, "BVC");
        self.add_branch_opcode(arch, 0x70, "BVS");
        self.add_imm_u8_opcode(arch, 0x00, "BRK");
        self.add_no_arg_opcode(arch, 0x18, "CLC");
        self.add_no_arg_opcode(arch, 0xd8, "CLD");
        self.add_no_arg_opcode(arch, 0x58, "CLI");
        self.add_no_arg_opcode(arch, 0xb8, "CLV");
        self.add_no_arg_opcode(arch, 0xca, "DEX");
        self.add_no_arg_opcode(arch, 0x88, "DEY");
        self.add_no_arg_opcode(arch, 0xe8, "INX");
        self.add_no_arg_opcode(arch, 0xc8, "INY");
        self.add_addr16_opcode(arch, 0x4c, "JMP");
        self.add_addr16_opcode(arch, 0x20, "JSR");
        self.add_accum_opcode(arch, 0x4a, "LSR");
        self.add_no_arg_opcode(arch, 0xea, "NOP");
        self.add_no_arg_opcode(arch, 0x48, "PHA");
        self.add_no_arg_opcode(arch, 0x08, "PHP");
        self.add_no_arg_opcode(arch, 0x68, "PLA");
        self.add_no_arg_opcode(arch, 0x28, "PLP");
        self.add_accum_opcode(arch, 0x2a, "ROL");
        self.add_accum_opcode(arch, 0x6a, "ROR");
        self.add_no_arg_opcode(arch, 0x40, "RTI");
        self.add_no_arg_opcode(arch, 0x60, "RTS");
        self.add_no_arg_opcode(arch, 0x38, "SEC");
        self.add_no_arg_opcode(arch, 0xf8, "SED");
        self.add_no_arg_opcode(arch, 0x78, "SEI");
        self.add_addr16_opcode(arch, 0x8d, "STA");
        self.add_addr16_opcode(arch, 0x8e, "STX");
        self.add_addr16_opcode(arch, 0x8c, "STY");
        self.add_no_arg_opcode(arch, 0xaa, "TAX");
        self.add_no_arg_opcode(arch, 0xa8, "TAY");
        self.add_no_arg_opcode(arch, 0xba, "TSX");
        self.add_no_arg_opcode(arch, 0x8a, "TXA");
        self.add_no_arg_opcode(arch, 0x9a, "TXS");
        self.add_no_arg_opcode(arch, 0x98, "TYA");
    }

    fn add_6502_macros(&mut self, arch: &Rc<str>) {
        self.add_imm_u8_opcode(arch, 0x69, "ADC");
        self.add_imm_u8_opcode(arch, 0x29, "AND");
        self.add_imm_u8_opcode(arch, 0xc9, "CMP");
        self.add_imm_u8_opcode(arch, 0xe0, "CPX");
        self.add_imm_u8_opcode(arch, 0xc0, "CPY");
        self.add_imm_u8_opcode(arch, 0x49, "EOR");
        self.add_imm_u8_opcode(arch, 0xa9, "LDA");
        self.add_imm_u8_opcode(arch, 0xa2, "LDX");
        self.add_imm_u8_opcode(arch, 0xa0, "LDY");
        self.add_imm_u8_opcode(arch, 0x09, "ORA");
        self.add_imm_u8_opcode(arch, 0xe9, "SBC");
    }

    fn add_65c816_macros(&mut self, arch: &Rc<str>) {
        self.add_branch_opcode(arch, 0x80, "BRA");
        self.add_imm_u8_opcode(arch, 0x02, "COP");
        self.add_accum_opcode(arch, 0x3a, "DEC");
        self.add_accum_opcode(arch, 0x1a, "INC");
        self.add_no_arg_opcode(arch, 0x8b, "PHB");
        self.add_no_arg_opcode(arch, 0x0b, "PHD");
        self.add_no_arg_opcode(arch, 0x4b, "PHK");
        self.add_no_arg_opcode(arch, 0xda, "PHX");
        self.add_no_arg_opcode(arch, 0x5a, "PHY");
        self.add_no_arg_opcode(arch, 0xab, "PLB");
        self.add_no_arg_opcode(arch, 0x2b, "PLD");
        self.add_no_arg_opcode(arch, 0xfa, "PLX");
        self.add_no_arg_opcode(arch, 0x7a, "PLY");
        self.add_imm_u8_opcode(arch, 0xc2, "REP");
        self.add_no_arg_opcode(arch, 0x6b, "RTL");
        self.add_imm_u8_opcode(arch, 0xe2, "SEP");
        self.add_no_arg_opcode(arch, 0xdb, "STP");
        self.add_addr16_opcode(arch, 0x9c, "STZ");
        self.add_no_arg_opcode(arch, 0x5b, "TCD");
        self.add_no_arg_opcode(arch, 0x1b, "TCS");
        self.add_no_arg_opcode(arch, 0x7b, "TDC");
        self.add_no_arg_opcode(arch, 0x3b, "TSC");
        self.add_no_arg_opcode(arch, 0x9b, "TXY");
        self.add_no_arg_opcode(arch, 0xbb, "TYX");
        self.add_no_arg_opcode(arch, 0xcb, "WAI");
        self.add_imm_u8_opcode(arch, 0x42, "WDM");
        self.add_no_arg_opcode(arch, 0xeb, "XBA");
        self.add_no_arg_opcode(arch, 0xfb, "XCE");
    }

    fn add_sm83_macros(&mut self, arch: &Rc<str>) {
        self.add_label16_opcode(arch, 0xcd, "CALL");
        self.add_no_arg_opcode(arch, 0x3d, "CCF");
        self.add_no_arg_opcode(arch, 0x2f, "CPL");
        self.add_no_arg_opcode(arch, 0x27, "DAA");
        self.add_no_arg_opcode(arch, 0xf3, "DI");
        self.add_no_arg_opcode(arch, 0xfb, "EI");
        self.add_no_arg_opcode(arch, 0x76, "HALT");
        self.add_label16_opcode(arch, 0xc3, "JP");
        self.add_branch_opcode(arch, 0x18, "JR");
        self.add_no_arg_opcode(arch, 0x00, "NOP");
        self.add_no_arg_opcode(arch, 0xc9, "RET");
        self.add_no_arg_opcode(arch, 0xd9, "RETI");
        self.add_no_arg_opcode(arch, 0x17, "RLA");
        self.add_no_arg_opcode(arch, 0x07, "RLCA");
        self.add_no_arg_opcode(arch, 0x1f, "RRA");
        self.add_no_arg_opcode(arch, 0x0f, "RRCA");
        self.add_no_arg_opcode(arch, 0x37, "SCF");
        self.add_no_arg_opcode(arch, 0x10, "STOP");
    }

    fn add_spc700_macros(&mut self, arch: &Rc<str>) {
        self.add_addr16_opcode(arch, 0x0c, "ASL");
        self.add_branch_opcode(arch, 0x90, "BCC");
        self.add_branch_opcode(arch, 0xb0, "BCS");
        self.add_branch_opcode(arch, 0xf0, "BEQ");
        self.add_branch_opcode(arch, 0x30, "BMI");
        self.add_branch_opcode(arch, 0xd0, "BNE");
        self.add_branch_opcode(arch, 0x10, "BPL");
        self.add_branch_opcode(arch, 0x2f, "BRA");
        self.add_branch_opcode(arch, 0x50, "BVC");
        self.add_branch_opcode(arch, 0x70, "BVS");
        self.add_no_arg_opcode(arch, 0x0f, "BRK");
        self.add_addr16_opcode(arch, 0x3f, "CALL");
        self.add_no_arg_opcode(arch, 0x60, "CLRC");
        self.add_no_arg_opcode(arch, 0x20, "CLRP");
        self.add_no_arg_opcode(arch, 0xe0, "CLRV");
        self.add_addr16_opcode(arch, 0x8c, "DEC");
        self.add_no_arg_opcode(arch, 0xc0, "DI");
        self.add_no_arg_opcode(arch, 0xa0, "EI");
        self.add_addr16_opcode(arch, 0xac, "INC");
        self.add_addr16_opcode(arch, 0x5f, "JMP");
        self.add_addr16_opcode(arch, 0x4c, "LSR");
        self.add_no_arg_opcode(arch, 0x00, "NOP");
        self.add_no_arg_opcode(arch, 0xed, "NOTC");
        self.add_no_arg_opcode(arch, 0x6f, "RET");
        self.add_no_arg_opcode(arch, 0x7f, "RETI");
        self.add_addr16_opcode(arch, 0x2c, "ROL");
        self.add_addr16_opcode(arch, 0x6c, "ROR");
        self.add_no_arg_opcode(arch, 0x80, "SETC");
        self.add_no_arg_opcode(arch, 0x40, "SETP");
        self.add_no_arg_opcode(arch, 0xef, "SLEEP");
        self.add_no_arg_opcode(arch, 0xff, "STOP");
        self.add_addr16_opcode(arch, 0x0e, "TSET1");
        self.add_addr16_opcode(arch, 0x4e, "TCLR1");
    }

    fn add_no_arg_opcode(
        &mut self,
        arch: &Rc<str>,
        opcode_byte: u8,
        name: &str,
    ) {
        let definition = AsmDefMacroAst {
            id: builtin_id(name),
            params: vec![],
            body: vec![constant_u8(opcode_byte)],
        };
        self.add_macro_definition(arch, definition);
    }

    fn add_accum_opcode(
        &mut self,
        arch: &Rc<str>,
        opcode_byte: u8,
        name: &str,
    ) {
        let definition = AsmDefMacroAst {
            id: builtin_id(name),
            params: vec![self.register_macro_arg(&self.reg_a)],
            body: vec![constant_u8(opcode_byte)],
        };
        self.add_macro_definition(arch, definition);
    }

    fn add_imm_u8_opcode(
        &mut self,
        arch: &Rc<str>,
        opcode_byte: u8,
        name: &str,
    ) {
        let definition = AsmDefMacroAst {
            id: builtin_id(name),
            params: vec![self.pound_imm_macro_arg()],
            body: vec![
                constant_u8(opcode_byte),
                placeholder_u8(&self.placeholder_imm),
            ],
        };
        self.add_macro_definition(arch, definition);
    }

    fn add_branch_opcode(
        &mut self,
        arch: &Rc<str>,
        opcode_byte: u8,
        name: &str,
    ) {
        let definition = AsmDefMacroAst {
            id: builtin_id(name),
            params: vec![self.addr_macro_arg()],
            body: vec![
                constant_u8(opcode_byte),
                relative_addr(&self.placeholder_addr),
            ],
        };
        self.add_macro_definition(arch, definition);
    }

    fn add_addr16_opcode(
        &mut self,
        arch: &Rc<str>,
        opcode_byte: u8,
        name: &str,
    ) {
        let definition = AsmDefMacroAst {
            id: builtin_id(name),
            params: vec![self.bang_addr_macro_arg()],
            body: vec![
                constant_u8(opcode_byte),
                placeholder_u16le(&self.placeholder_addr),
            ],
        };
        self.add_macro_definition(arch, definition);
    }

    fn add_label16_opcode(
        &mut self,
        arch: &Rc<str>,
        opcode_byte: u8,
        name: &str,
    ) {
        let definition = AsmDefMacroAst {
            id: builtin_id(name),
            params: vec![self.addr_macro_arg()],
            body: vec![
                constant_u8(opcode_byte),
                placeholder_u16le(&self.placeholder_addr),
            ],
        };
        self.add_macro_definition(arch, definition);
    }

    fn add_macro_definition(
        &mut self,
        arch: &Rc<str>,
        definition: AsmDefMacroAst,
    ) {
        let reserved = self.arch_tree.reserved_names(arch);
        self.macros.define(arch, reserved, definition).unwrap();
    }

    fn addr_macro_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![token(TokenValue::Placeholder(
                self.placeholder_addr.clone(),
            ))],
        }
    }

    fn bang_addr_macro_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Bang),
                token(TokenValue::Placeholder(self.placeholder_addr.clone())),
            ],
        }
    }

    fn pound_imm_macro_arg(&self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![
                token(TokenValue::Pound),
                token(TokenValue::Placeholder(self.placeholder_imm.clone())),
            ],
        }
    }

    fn register_macro_arg(&self, name: &Rc<str>) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::BUILTIN,
            tokens: vec![token(TokenValue::Identifier(name.clone()))],
        }
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
