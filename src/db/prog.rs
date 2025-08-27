use super::env::SimEnv;
use super::expr::{AdsDecl, AdsDeclKind, AdsTypeEnv};
use super::inst::{AdsBreakpointKind, AdsFrameRef, AdsInstruction};
use super::value::{AdsType, AdsValue};
use crate::parse::{
    AdsModuleAst, AdsStmtAst, BreakpointAst, DeclareAst, ExprAst,
    IdentifierAst, ParseError, SrcSpan,
};

//===========================================================================//

/// An executable Atma Debugger Script program.
pub struct AdsProgram {
    pub(crate) instructions: Vec<AdsInstruction>,
}

impl AdsProgram {
    /// Reads an Atma Debugger Script program from a file.
    pub fn compile_source(
        source: &str,
        sim_env: &SimEnv,
    ) -> Result<AdsProgram, Vec<ParseError>> {
        AdsProgram::compile_ast(AdsModuleAst::parse_source(source)?, sim_env)
    }

    /// Distills the abstract syntax tree for an Atma Debugger Script module
    /// into a program that can be executed.
    fn compile_ast(
        module: AdsModuleAst,
        sim_env: &SimEnv,
    ) -> Result<AdsProgram, Vec<ParseError>> {
        let mut compiler = AdsCompiler::new(sim_env);
        let mut instructions = Vec::<AdsInstruction>::new();
        compiler.typecheck_statements(module.statements, &mut instructions);
        let errors = compiler.into_errors();
        if errors.is_empty() {
            if !matches!(instructions.last(), Some(AdsInstruction::Exit)) {
                instructions.push(AdsInstruction::Exit);
            }
            Ok(AdsProgram { instructions })
        } else {
            Err(errors)
        }
    }
}

//===========================================================================//

struct AdsCompiler<'a> {
    env: AdsTypeEnv<'a>,
    errors: Vec<ParseError>,
}

impl<'a> AdsCompiler<'a> {
    fn new(sim_env: &'a SimEnv) -> AdsCompiler<'a> {
        AdsCompiler { env: AdsTypeEnv::new(sim_env), errors: Vec::new() }
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
        out: &mut Vec<AdsInstruction>,
    ) {
        match statement {
            AdsStmtAst::Declare(kind, id, expr_ast) => {
                self.typecheck_declare_statement(kind, id, expr_ast, out);
            }
            AdsStmtAst::Exit => out.push(AdsInstruction::Exit),
            AdsStmtAst::If(pred_ast, then_ast, else_ast) => {
                self.typecheck_if_statement(pred_ast, then_ast, else_ast, out);
            }
            AdsStmtAst::Print(expr_ast) => {
                self.typecheck_print_statement(expr_ast, out);
            }
            AdsStmtAst::Relax => {}
            AdsStmtAst::RunUntil(breakpoint_ast) => {
                self.typecheck_run_until_statement(breakpoint_ast, out);
            }
            AdsStmtAst::Set(id, expr_ast) => {
                self.typecheck_set_statement(id, expr_ast, out);
            }
            AdsStmtAst::Step => out.push(AdsInstruction::Step),
            AdsStmtAst::When(breakpoint_ast, do_ast) => {
                self.typecheck_when_statement(breakpoint_ast, do_ast, out);
            }
        }
    }

    fn typecheck_declare_statement(
        &mut self,
        kind: DeclareAst,
        id: IdentifierAst,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) {
        let (expr_type, static_value) =
            if let Some((mut ops, ty)) = self.typecheck_expr(expr_ast) {
                let static_value = self.try_get_static(&ops);
                out.append(&mut ops);
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

    fn typecheck_if_statement(
        &mut self,
        pred_ast: ExprAst,
        then_ast: Vec<AdsStmtAst>,
        else_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) {
        let pred_span = pred_ast.span;
        if let Some((mut ops, expr_type)) = self.typecheck_expr(pred_ast) {
            if let AdsType::Boolean = expr_type {
                out.append(&mut ops);
            } else {
                let message =
                    format!("predicate must be of type bool, not {expr_type}");
                let label = format!("this expression has type {expr_type}");
                self.errors.push(
                    ParseError::new(pred_span, message)
                        .with_label(pred_span, label),
                );
            }
        }
        self.env.push_scope();
        let mut then_stmts = Vec::<AdsInstruction>::new();
        self.typecheck_statements(then_ast, &mut then_stmts);
        self.env.pop_scope(&mut then_stmts);
        self.env.push_scope();
        let mut else_stmts = Vec::<AdsInstruction>::new();
        self.typecheck_statements(else_ast, &mut else_stmts);
        self.env.pop_scope(&mut else_stmts);
        if !else_stmts.is_empty() {
            then_stmts.push(AdsInstruction::Jump(else_stmts.len() as isize));
        }
        out.push(AdsInstruction::BranchUnless(then_stmts.len() as isize));
        out.append(&mut then_stmts);
        out.append(&mut else_stmts);
    }

    fn typecheck_print_statement(
        &mut self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) {
        if let Some((mut ops, _)) = self.typecheck_expr(expr_ast) {
            out.append(&mut ops);
            out.push(AdsInstruction::Print);
        }
    }

    fn typecheck_run_until_statement(
        &mut self,
        breakpoint_ast: BreakpointAst,
        out: &mut Vec<AdsInstruction>,
    ) {
        if let Some((mut breakpoint_ops, breakpoint_kind)) =
            self.typecheck_breakpoint(breakpoint_ast)
        {
            let (outer_ref, inner_ref) = if self.env.in_global_frame() {
                (AdsFrameRef::Global, AdsFrameRef::Global)
            } else {
                (AdsFrameRef::Local(0), AdsFrameRef::Local(1))
            };
            let index = self.env.frame_end();
            out.push(AdsInstruction::PushValue(AdsValue::Boolean(false)));
            out.append(&mut breakpoint_ops);
            out.push(AdsInstruction::PushHandler(breakpoint_kind, 1));
            out.push(AdsInstruction::Jump(3));
            out.push(AdsInstruction::PushValue(AdsValue::Boolean(true)));
            out.push(AdsInstruction::SetValue(inner_ref, index));
            out.push(AdsInstruction::Return);
            out.push(AdsInstruction::Step);
            out.push(AdsInstruction::GetValue(outer_ref, index));
            out.push(AdsInstruction::BranchUnless(-3));
            out.push(AdsInstruction::PopHandler);
            out.push(AdsInstruction::PopValue);
        }
    }

    fn typecheck_set_statement(
        &mut self,
        id: IdentifierAst,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) {
        let id_name = id.name;
        let expr_span = expr_ast.span;
        let opt_expr = self.typecheck_expr(expr_ast);
        match self.env.get_declaration(&id_name) {
            Some((
                frame_ref,
                decl @ AdsDecl { kind: AdsDeclKind::Variable, .. },
            )) => {
                if let Some((mut ops, expr_type)) = opt_expr {
                    if expr_type == decl.var_type {
                        out.append(&mut ops);
                        out.push(AdsInstruction::SetValue(
                            frame_ref,
                            decl.stack_index,
                        ));
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
            Some((
                _,
                decl @ AdsDecl { kind: AdsDeclKind::Constant(_), .. },
            )) => {
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
                let uppercase = id_name.to_ascii_uppercase();
                for &reg in self.env.register_names() {
                    if uppercase == reg {
                        if let Some((mut ops, expr_type)) = opt_expr {
                            if let AdsType::Integer = expr_type {
                                out.append(&mut ops);
                                out.push(AdsInstruction::SetRegister(reg));
                            } else {
                                let message = format!(
                                    "cannot assign {expr_type} value to \
                                     simulated processor register"
                                );
                                let label = format!(
                                    "this expression has type {expr_type}"
                                );
                                self.errors.push(
                                    ParseError::new(id.span, message)
                                        .with_label(expr_span, label),
                                );
                            }
                        }
                        return;
                    }
                }
                if uppercase == "PC" {
                    if let Some((mut ops, expr_type)) = opt_expr {
                        if let AdsType::Integer = expr_type {
                            out.append(&mut ops);
                            out.push(AdsInstruction::SetPc);
                        } else {
                            let message = format!(
                                "cannot assign {expr_type} value to simulated \
                                 processor program counter"
                            );
                            let label = format!(
                                "this expression has type {expr_type}"
                            );
                            self.errors.push(
                                ParseError::new(id.span, message)
                                    .with_label(expr_span, label),
                            );
                        }
                    }
                } else {
                    let message = format!("no such variable: `{id_name}`");
                    let label = "this was never declared".to_string();
                    self.errors.push(
                        ParseError::new(id.span, message)
                            .with_label(id.span, label),
                    );
                }
            }
        }
    }

    fn typecheck_when_statement(
        &mut self,
        breakpoint_ast: BreakpointAst,
        do_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) {
        if let Some((mut ops, kind)) =
            self.typecheck_breakpoint(breakpoint_ast)
        {
            out.append(&mut ops);
            out.push(AdsInstruction::PushHandler(kind, 1));
        }
        self.env.push_frame();
        let mut do_stmts = Vec::<AdsInstruction>::new();
        self.typecheck_statements(do_ast, &mut do_stmts);
        self.env.pop_frame(&mut do_stmts);
        do_stmts.push(AdsInstruction::Return);
        self.env.add_handler();
        out.push(AdsInstruction::Jump(do_stmts.len() as isize));
        out.append(&mut do_stmts);
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
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{
        AdsBreakpointKind, AdsFrameRef, AdsInstruction, AdsProgram, AdsValue,
        SimEnv,
    };
    use crate::bus::null_bus;
    use crate::proc::Mos6502;
    use num_bigint::BigInt;

    fn compile(source: &str) -> Vec<AdsInstruction> {
        let cpu = Mos6502::new(null_bus());
        let sim_env = SimEnv::new(vec![("cpu".to_string(), Box::new(cpu))]);
        AdsProgram::compile_source(source, &sim_env).unwrap().instructions
    }

    fn int_value(value: i32) -> AdsValue {
        AdsValue::Integer(BigInt::from(value))
    }

    #[test]
    fn empty_program() {
        assert_eq!(compile(""), vec![AdsInstruction::Exit]);
    }

    #[test]
    fn exit_instruction_only() {
        assert_eq!(compile("exit\n"), vec![AdsInstruction::Exit]);
    }

    #[test]
    fn if_instruction() {
        assert_eq!(
            compile("if %false {\nstep\n}\n"),
            vec![
                AdsInstruction::PushValue(AdsValue::Boolean(false)),
                AdsInstruction::BranchUnless(1),
                AdsInstruction::Step,
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn if_else_instruction() {
        assert_eq!(
            compile("if %false {\nprint 1\n} else {\nprint 2\n}\n"),
            vec![
                AdsInstruction::PushValue(AdsValue::Boolean(false)),
                AdsInstruction::BranchUnless(3),
                AdsInstruction::PushValue(int_value(1)),
                AdsInstruction::Print,
                AdsInstruction::Jump(2),
                AdsInstruction::PushValue(int_value(2)),
                AdsInstruction::Print,
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn let_instruction() {
        assert_eq!(
            compile("let x = 5\n"),
            vec![
                AdsInstruction::PushValue(int_value(5)),
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn let_instruction_with_pc() {
        assert_eq!(
            compile("let x = pc\n"),
            vec![AdsInstruction::GetPc, AdsInstruction::Exit]
        );
    }

    #[test]
    fn print_instruction() {
        assert_eq!(
            compile("print 42\n"),
            vec![
                AdsInstruction::PushValue(int_value(42)),
                AdsInstruction::Print,
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn relax_instruction() {
        assert_eq!(compile("relax\n"), vec![AdsInstruction::Exit]);
    }

    #[test]
    fn set_instruction() {
        assert_eq!(
            compile("var x = 1\nset x = 2\n"),
            vec![
                AdsInstruction::PushValue(int_value(1)),
                AdsInstruction::PushValue(int_value(2)),
                AdsInstruction::SetValue(AdsFrameRef::Global, 0),
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn step_instruction() {
        assert_eq!(
            compile("step\n"),
            vec![AdsInstruction::Step, AdsInstruction::Exit]
        );
    }

    #[test]
    fn when_instruction() {
        assert_eq!(
            compile("when at $ff {\nprint 1\n}\nstep\n"),
            vec![
                AdsInstruction::PushValue(int_value(255)),
                AdsInstruction::PushHandler(AdsBreakpointKind::Pc, 1),
                AdsInstruction::Jump(3),
                AdsInstruction::PushValue(int_value(1)),
                AdsInstruction::Print,
                AdsInstruction::Return,
                AdsInstruction::Step,
                AdsInstruction::Exit,
            ]
        );
    }
}

//===========================================================================//
