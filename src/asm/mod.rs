//! Facilities for assembling source files into object files.

mod expr;

use crate::expr::ExprType;
use crate::obj::{Align32, BinaryIo, ObjectChunk};
use crate::parse::{
    AsmModuleAst, AsmSectionAst, AsmStmtAst, ExprAst, ParseError,
};
pub use expr::AsmExpr;
use expr::AsmTypeEnv;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// Represents an object file assembled from a source file.
pub struct ObjectFile {
    /// The section chunks to be linked.
    pub chunks: Vec<ObjectChunk>,
}

impl ObjectFile {
    /// Assembles an object file from source code.
    pub fn assemble_source(
        source: &str,
    ) -> Result<ObjectFile, Vec<ParseError>> {
        ObjectFile::assemble_ast(AsmModuleAst::parse_source(source)?)
    }

    fn assemble_ast(
        module: AsmModuleAst,
    ) -> Result<ObjectFile, Vec<ParseError>> {
        let mut assembler = Assembler::new();
        assembler.visit_module(&module);
        assembler.finish()
    }
}

impl BinaryIo for ObjectFile {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let chunks = Vec::<ObjectChunk>::read_from(reader)?;
        Ok(ObjectFile { chunks })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.chunks.write_to(writer)?;
        Ok(())
    }
}

//===========================================================================//

struct Assembler {
    chunks: Vec<ObjectChunk>,
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
            AsmStmtAst::Label(_id) => {}     // TODO
            AsmStmtAst::Section(section) => self.visit_section(section),
            AsmStmtAst::U8(_expr) => {} // TODO
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
        let align = Align32::default(); // TODO: support align attribute
        let within: Option<Align32> = None; // TODO: support within attribute
        self.section_stack.push(SectionEnv::new());
        self.visit_statements(&section_ast.body);
        debug_assert!(!self.section_stack.is_empty());
        let section_env = self.section_stack.pop().unwrap();
        // TODO: error if size is too large
        let size = section_env.data.len() as u32;
        if let Some(section_name) = name {
            self.chunks.push(ObjectChunk {
                section_name,
                data: Rc::from(section_env.data),
                size,
                align,
                within,
            });
        }
    }

    fn typecheck_expression(
        &mut self,
        expr_ast: &ExprAst,
    ) -> Option<(AsmExpr, ExprType)> {
        let env = AsmTypeEnv {};
        match env.typecheck_expression(expr_ast) {
            Ok(expr_and_type) => Some(expr_and_type),
            Err(errors) => {
                self.errors.extend(errors);
                None
            }
        }
    }

    fn finish(self) -> Result<ObjectFile, Vec<ParseError>> {
        if self.errors.is_empty() {
            Ok(ObjectFile { chunks: self.chunks })
        } else {
            Err(self.errors)
        }
    }
}

//===========================================================================//

struct SectionEnv {
    data: Vec<u8>,
}

impl SectionEnv {
    fn new() -> SectionEnv {
        SectionEnv { data: Vec::new() }
    }
}

//===========================================================================//
