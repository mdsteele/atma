//! Facilities for assembling source files into object files.

mod expr;

use crate::addr::{Align, Offset, Size};
use crate::expr::ExprType;
use crate::obj::{ObjChunk, ObjExpr, ObjFile, ObjPatch, ObjSymbol, PatchKind};
use crate::parse::{
    AsmModuleAst, AsmSectionAst, AsmStmtAst, ExprAst, IdentifierAst,
    ParseError, ParseResult,
};
use expr::AsmTypeEnv;
use std::rc::Rc;

//===========================================================================//

/// Assembles an object file from source code.
pub fn assemble_source(source: &str) -> ParseResult<ObjFile> {
    assemble_ast(AsmModuleAst::parse_source(source)?)
}

fn assemble_ast(module: AsmModuleAst) -> ParseResult<ObjFile> {
    let mut assembler = Assembler::new();
    assembler.visit_module(&module);
    assembler.finish()
}

//===========================================================================//

struct Assembler {
    chunks: Vec<ObjChunk>,
    errors: Vec<ParseError>,
    section_stack: Vec<SectionEnv>,
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            chunks: Vec::new(),
            errors: Vec::new(),
            section_stack: Vec::new(),
        }
    }

    fn visit_module(&mut self, module: &AsmModuleAst) {
        self.visit_statements(&module.statements);
    }

    fn visit_statements(&mut self, statements: &[AsmStmtAst]) {
        for statement in statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &AsmStmtAst) {
        match statement {
            AsmStmtAst::Invoke(_macro) => {} // TODO
            AsmStmtAst::Label(id) => self.visit_label(id),
            AsmStmtAst::Section(section) => self.visit_section(section),
            AsmStmtAst::U8(expr) => self.visit_u8(expr),
            AsmStmtAst::U16le(expr) => self.visit_u16le(expr),
            AsmStmtAst::U24le(expr) => self.visit_u24le(expr),
        }
    }

    fn visit_label(&mut self, id_ast: &IdentifierAst) {
        if let Some(section_env) = self.section_stack.last_mut() {
            section_env.symbols.push(ObjSymbol {
                name: id_ast.name.clone(),
                exported: false,
                offset: Offset::try_from(section_env.data.len()).unwrap(),
            });
        } else {
            let message = "labels must be within a .SECTION".to_string();
            self.errors.push(ParseError::new(id_ast.span, message));
        }
    }

    fn visit_section(&mut self, section_ast: &AsmSectionAst) {
        let name: Option<Rc<str>> = match self
            .typecheck_expression(&section_ast.name)
        {
            Some((expr, ExprType::String)) => match expr.static_value() {
                Some(value) => Some(value.clone().unwrap_str()),
                None => {
                    let message = "section name must be static".to_string();
                    let label = "this expression isn't static".to_string();
                    self.errors.push(
                        ParseError::new(section_ast.name.span, message)
                            .with_label(section_ast.name.span, label),
                    );
                    None
                }
            },
            Some((_, ty)) => {
                let message = "section name must be a string".to_string();
                let label = format!("this expression has type {ty}");
                self.errors.push(
                    ParseError::new(section_ast.name.span, message)
                        .with_label(section_ast.name.span, label),
                );
                None
            }
            None => None,
        };
        let align = Align::default(); // TODO: support align attribute
        let within: Option<Align> = None; // TODO: support within attribute
        let fill: Option<u8> = None; // TODO: support fill attribute
        self.section_stack.push(SectionEnv::new());
        self.visit_statements(&section_ast.body);
        debug_assert!(!self.section_stack.is_empty());
        let section_env = self.section_stack.pop().unwrap();
        // TODO: error if size is too large
        let size = Size::try_from(section_env.data.len()).unwrap();
        if let Some(section_name) = name {
            self.chunks.push(ObjChunk {
                section_name,
                data: Box::from(section_env.data),
                size,
                align,
                within,
                fill,
                symbols: Rc::from(section_env.symbols),
                patches: Box::from(section_env.patches),
            });
        }
    }

    fn visit_u16le(&mut self, expr_ast: &ExprAst) {
        let word = self.visit_int_data_directive(PatchKind::U16le, expr_ast);
        let section_env = self.section_stack.last_mut().unwrap();
        section_env.data.push(word as u8);
        section_env.data.push((word >> 8) as u8);
    }

    fn visit_u24le(&mut self, expr_ast: &ExprAst) {
        let long = self.visit_int_data_directive(PatchKind::U24le, expr_ast);
        let section_env = self.section_stack.last_mut().unwrap();
        section_env.data.push(long as u8);
        section_env.data.push((long >> 8) as u8);
        section_env.data.push((long >> 16) as u8);
    }

    fn visit_u8(&mut self, expr_ast: &ExprAst) {
        let byte = self.visit_int_data_directive(PatchKind::U8, expr_ast);
        if let Some(section_env) = self.section_stack.last_mut() {
            section_env.data.push(byte as u8);
        }
    }

    fn visit_int_data_directive(
        &mut self,
        kind: PatchKind,
        expr_ast: &ExprAst,
    ) -> i64 {
        if self.section_stack.is_empty() {
            let message = format!(
                "{} directive must be within a .SECTION",
                kind.directive()
            );
            self.errors.push(ParseError::new(expr_ast.span, message));
        }
        match self.typecheck_expression(expr_ast) {
            Some((expr, ExprType::Integer)) => match expr.static_value() {
                Some(value) => {
                    let bigint = value.unwrap_int_ref();
                    kind.value_in_range(bigint).unwrap_or_else(|range| {
                        let message = format!(
                            "{} value is statically out of range ({}-{})",
                            kind.directive(),
                            range.start(),
                            range.end()
                        );
                        let label = format!(
                            "the value of this expression is {bigint}"
                        );
                        self.errors.push(
                            ParseError::new(expr_ast.span, message)
                                .with_label(expr_ast.span, label),
                        );
                        0
                    })
                }
                None => {
                    if let Some(section_env) = self.section_stack.last_mut() {
                        // TODO: Error instead of crash if offset is too large.
                        let offset =
                            Offset::try_from(section_env.data.len()).unwrap();
                        let patch = ObjPatch { offset, kind, expr };
                        section_env.patches.push(patch);
                    }
                    0
                }
            },
            Some((_, ty)) => {
                let message =
                    format!("{} value must be an integer", kind.directive());
                let label = format!("this expression has type {ty}");
                self.errors.push(
                    ParseError::new(expr_ast.span, message)
                        .with_label(expr_ast.span, label),
                );
                0
            }
            None => 0,
        }
    }

    fn typecheck_expression(
        &mut self,
        expr_ast: &ExprAst,
    ) -> Option<(ObjExpr, ExprType)> {
        let env = AsmTypeEnv {};
        match env.typecheck_expression(expr_ast) {
            Ok(expr_and_type) => Some(expr_and_type),
            Err(errors) => {
                self.errors.extend(errors);
                None
            }
        }
    }

    fn finish(self) -> ParseResult<ObjFile> {
        if self.errors.is_empty() {
            Ok(ObjFile { chunks: self.chunks })
        } else {
            Err(self.errors)
        }
    }
}

//===========================================================================//

struct SectionEnv {
    data: Vec<u8>,
    symbols: Vec<ObjSymbol>,
    patches: Vec<ObjPatch>,
}

impl SectionEnv {
    fn new() -> SectionEnv {
        SectionEnv {
            data: Vec::new(),
            symbols: Vec::new(),
            patches: Vec::new(),
        }
    }
}

//===========================================================================//
