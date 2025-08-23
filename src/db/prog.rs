use super::expr::{AdsDecl, AdsDeclKind, AdsTypeEnv};
use super::inst::{AdsBreakpointKind, AdsInstruction};
use super::value::{AdsType, AdsValue};
use crate::parse::{
    AdsModuleAst, AdsStmtAst, BreakpointAst, DeclareAst, ExprAst,
    IdentifierAst, ParseError, SrcSpan,
};
use std::io::Read;

//===========================================================================//

/// An executable Atma Debugger Script program.
pub struct AdsProgram {
    pub(crate) instructions: Vec<AdsInstruction>,
}

impl AdsProgram {
    /// Reads an Atma Debugger Script program from a file.
    pub fn read_from<R: Read>(
        reader: R,
    ) -> Result<AdsProgram, Vec<ParseError>> {
        AdsProgram::typecheck(AdsModuleAst::read_from(reader)?)
    }

    /// Distills the abstract syntax tree for an Atma Debugger Script module
    /// into a program that can be executed.
    pub fn typecheck(
        module: AdsModuleAst,
    ) -> Result<AdsProgram, Vec<ParseError>> {
        let mut compiler = AdsCompiler::new();
        let mut instructions = Vec::<AdsInstruction>::new();
        compiler.typecheck_statements(module.statements, &mut instructions);
        let errors = compiler.into_errors();
        if errors.is_empty() {
            Ok(AdsProgram { instructions })
        } else {
            Err(errors)
        }
    }
}

//===========================================================================//

struct AdsCompiler {
    env: AdsTypeEnv,
    errors: Vec<ParseError>,
}

impl AdsCompiler {
    fn new() -> AdsCompiler {
        AdsCompiler { env: AdsTypeEnv::empty(), errors: Vec::new() }
    }

    fn into_errors(self) -> Vec<ParseError> {
        self.errors
    }

    fn typecheck_statements(
        &mut self,
        statements: Vec<AdsStmtAst>,
        instructions_out: &mut Vec<AdsInstruction>,
    ) {
        for statement in statements {
            self.typecheck_statement(statement, instructions_out);
        }
    }

    fn try_get_static(
        &self,
        instructions: &[AdsInstruction],
    ) -> Option<AdsValue> {
        match instructions {
            [AdsInstruction::PushValue(value)] => Some(value.clone()),
            _ => None,
        }
    }

    fn typecheck_statement(
        &mut self,
        statement: AdsStmtAst,
        instructions_out: &mut Vec<AdsInstruction>,
    ) {
        match statement {
            AdsStmtAst::Relax => {}
            AdsStmtAst::Exit => instructions_out.push(AdsInstruction::Exit),
            AdsStmtAst::Step => instructions_out.push(AdsInstruction::Step),
            AdsStmtAst::Print(expr) => {
                if let Some((mut ops, _)) = self.typecheck_expr(expr) {
                    instructions_out.append(&mut ops);
                    instructions_out.push(AdsInstruction::Print);
                }
            }
            AdsStmtAst::Set(id, expr_ast) => {
                self.typecheck_set_statement(id, expr_ast, instructions_out);
            }
            AdsStmtAst::Declare(kind, id, expr) => {
                let (expr_type, static_value) =
                    if let Some((mut ops, ty)) = self.typecheck_expr(expr) {
                        let static_value = self.try_get_static(&ops);
                        instructions_out.append(&mut ops);
                        (ty, static_value)
                    } else {
                        (AdsType::Bottom, None)
                    };
                let kind = match kind {
                    DeclareAst::Let => AdsDeclKind::Constant(static_value),
                    DeclareAst::Var => AdsDeclKind::Variable,
                };
                self.env.add_declaration(kind, id, expr_type);
            }
            AdsStmtAst::If(pred_ast, then_ast, else_ast) => {
                let pred_span = pred_ast.span;
                if let Some((mut ops, expr_type)) =
                    self.typecheck_expr(pred_ast)
                {
                    if let AdsType::Boolean = expr_type {
                        instructions_out.append(&mut ops);
                    } else {
                        let message = format!(
                            "predicate must be of type bool, not {expr_type}"
                        );
                        self.errors.push(
                            ParseError::new(pred_span, message).with_label(
                                pred_span,
                                format!(
                                    "this expression has type {expr_type}"
                                ),
                            ),
                        );
                    }
                }
                self.push_scope();
                let mut then_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(then_ast, &mut then_stmts);
                self.pop_scope(&mut then_stmts);
                self.push_scope();
                let mut else_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(else_ast, &mut else_stmts);
                self.pop_scope(&mut else_stmts);
                if !else_stmts.is_empty() {
                    then_stmts
                        .push(AdsInstruction::Jump(else_stmts.len() as isize));
                }
                instructions_out.push(AdsInstruction::BranchUnless(
                    then_stmts.len() as isize,
                ));
                instructions_out.append(&mut then_stmts);
                instructions_out.append(&mut else_stmts);
            }
            AdsStmtAst::RunUntil(breakpoint_ast) => {
                if let Some((mut ops, kind)) =
                    self.typecheck_breakpoint(breakpoint_ast)
                {
                    let index = self.env.variable_stack_size();
                    instructions_out.push(AdsInstruction::PushValue(
                        AdsValue::Boolean(false),
                    ));
                    instructions_out.append(&mut ops);
                    instructions_out
                        .push(AdsInstruction::PushHandler(kind, 1));
                    instructions_out.push(AdsInstruction::Jump(3));
                    instructions_out.push(AdsInstruction::PushValue(
                        AdsValue::Boolean(true),
                    ));
                    instructions_out.push(AdsInstruction::SetValue(index));
                    instructions_out.push(AdsInstruction::Return);
                    instructions_out.push(AdsInstruction::Step);
                    instructions_out.push(AdsInstruction::CopyValue(index));
                    instructions_out.push(AdsInstruction::BranchUnless(-3));
                    instructions_out.push(AdsInstruction::PopHandler);
                    instructions_out.push(AdsInstruction::PopValue);
                }
            }
            AdsStmtAst::When(breakpoint_ast, do_ast) => {
                if let Some((mut ops, kind)) =
                    self.typecheck_breakpoint(breakpoint_ast)
                {
                    instructions_out.append(&mut ops);
                    instructions_out
                        .push(AdsInstruction::PushHandler(kind, 1));
                }
                self.push_scope();
                let mut do_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(do_ast, &mut do_stmts);
                self.pop_scope(&mut do_stmts);
                do_stmts.push(AdsInstruction::Return);
                self.env.add_handler();
                instructions_out
                    .push(AdsInstruction::Jump(do_stmts.len() as isize));
                instructions_out.append(&mut do_stmts);
            }
        }
    }

    fn typecheck_set_statement(
        &mut self,
        id: IdentifierAst,
        expr_ast: ExprAst,
        instructions_out: &mut Vec<AdsInstruction>,
    ) {
        let id_name = id.name;
        let expr_span = expr_ast.span;
        let opt_expr = self.typecheck_expr(expr_ast);
        match self.env.get_declaration(&id_name) {
            Some(decl @ AdsDecl { kind: AdsDeclKind::Variable, .. }) => {
                if let Some((mut ops, expr_type)) = opt_expr {
                    if expr_type == decl.var_type {
                        instructions_out.append(&mut ops);
                    } else {
                        let var_type = &decl.var_type;
                        let message = format!(
                            "cannot assign {expr_type} value to {var_type} \
                             variable `{id_name}`"
                        );
                        let label1 =
                            format!("this expression has type {expr_type}");
                        let label2 = format!(
                            "`{id_name}` was declared as {var_type} here"
                        );
                        self.errors.push(
                            ParseError::new(id.span, message)
                                .with_label(expr_span, label1)
                                .with_label(decl.id_span, label2),
                        );
                    }
                }
            }
            Some(decl @ AdsDecl { kind: AdsDeclKind::Constant(_), .. }) => {
                let message =
                    format!("cannot change value of constant `{id_name}`");
                let label1 = format!("cannot set value of `{id_name}`");
                let label2 =
                    format!("`{id_name}` was declared as a constant here");
                self.errors.push(
                    ParseError::new(id.span, message)
                        .with_label(id.span, label1)
                        .with_label(decl.id_span, label2),
                );
            }
            None => {
                let message = format!("no such variable: `{id_name}`");
                let label = "this was never declared".to_string();
                self.errors.push(
                    ParseError::new(id.span, message)
                        .with_label(id.span, label),
                );
            }
        }
    }

    fn typecheck_expr(
        &mut self,
        ast: ExprAst,
    ) -> Option<(Vec<AdsInstruction>, AdsType)> {
        match self.env.typecheck_expression(ast) {
            Ok(ops_and_type) => Some(ops_and_type),
            Err(mut errors) => {
                self.errors.append(&mut errors);
                None
            }
        }
    }

    fn typecheck_breakpoint(
        &mut self,
        ast: BreakpointAst,
    ) -> Option<(Vec<AdsInstruction>, AdsBreakpointKind)> {
        match ast {
            BreakpointAst::Pc(expr_ast) => {
                let expr_span = expr_ast.span;
                if let Some((ops, expr_type)) = self.typecheck_expr(expr_ast)
                    && self.typecheck_breakpoint_addr(expr_span, expr_type)
                {
                    return Some((ops, AdsBreakpointKind::Pc));
                }
            }
        }
        None
    }

    fn typecheck_breakpoint_addr(
        &mut self,
        expr_span: SrcSpan,
        expr_type: AdsType,
    ) -> bool {
        if let AdsType::Integer = expr_type {
            return true;
        }
        let message =
            format!("breakpoint address must be of type int, not {expr_type}");
        let label = format!("this expression has type {expr_type}");
        self.errors.push(
            ParseError::new(expr_span, message).with_label(expr_span, label),
        );
        false
    }

    fn push_scope(&mut self) {
        self.env.push_scope();
    }

    fn pop_scope(&mut self, instructions_out: &mut Vec<AdsInstruction>) {
        let (num_handlers, num_variables) = self.env.pop_scope();
        for _ in 0..num_handlers {
            instructions_out.push(AdsInstruction::PopHandler);
        }
        for _ in 0..num_variables {
            instructions_out.push(AdsInstruction::PopValue);
        }
    }
}

//===========================================================================//
