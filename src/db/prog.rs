use super::env::SimEnv;
use super::error::{AdsError, AdsResult};
use super::expr::{AdsDecl, AdsDeclKind, AdsTypeEnv};
use super::inst::{AdsFrameRef, AdsInstruction};
use crate::bus::WatchKind;
use crate::error::{Errs, SrcCache, SrcSpan};
use crate::expr::{ExprType, ExprValue};
use crate::parse::{
    AdsModuleAst, AdsStmtAst, BreakpointAst, DeclareAst, ExprAst,
    IdentifierAst, LValueAst, LValueAstNode,
};
use std::path::Path;
use std::rc::Rc;

//===========================================================================//

/// An executable Atma Debugger Script program.
pub struct AdsProgram {
    pub(crate) instructions: Vec<AdsInstruction>,
}

impl AdsProgram {
    /// Reads an Atma Debugger Script program from a file.
    pub fn compile_source(
        cache: &mut dyn SrcCache,
        src_path: Rc<str>,
        source_code: &str,
        sim_env: &SimEnv,
    ) -> AdsResult<AdsProgram> {
        let ast =
            AdsModuleAst::parse_source(source_code).map_err(Errs::coerce)?;
        AdsProgram::compile_ast(cache, src_path, ast, sim_env)
    }

    /// Distills the abstract syntax tree for an Atma Debugger Script module
    /// into a program that can be executed.
    fn compile_ast(
        cache: &mut dyn SrcCache,
        src_path: Rc<str>,
        module: AdsModuleAst,
        sim_env: &SimEnv,
    ) -> AdsResult<AdsProgram> {
        let mut compiler = AdsCompiler::new(cache, src_path, sim_env);
        let mut instructions = Vec::<AdsInstruction>::new();
        compiler.typecheck_statements(module.statements, &mut instructions)?;
        if !matches!(instructions.last(), Some(AdsInstruction::Exit)) {
            instructions.push(AdsInstruction::Exit);
        }
        Ok(AdsProgram { instructions })
    }
}

//===========================================================================//

struct AdsCompiler<'a> {
    cache: &'a mut dyn SrcCache,
    src_path: Rc<str>,
    env: AdsTypeEnv<'a>,
}

impl<'a> AdsCompiler<'a> {
    fn new(
        cache: &'a mut dyn SrcCache,
        src_path: Rc<str>,
        sim_env: &'a SimEnv,
    ) -> AdsCompiler<'a> {
        AdsCompiler { cache, src_path, env: AdsTypeEnv::new(sim_env) }
    }

    fn typecheck_statements(
        &mut self,
        statements: Vec<AdsStmtAst>,
        instructions_out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        for statement in statements {
            errs.also(self.typecheck_statement(statement, instructions_out));
        }
        errs.result()
    }

    fn typecheck_statement(
        &mut self,
        statement: AdsStmtAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        match statement {
            AdsStmtAst::Declare(kind, id, expr_ast) => {
                self.typecheck_declare_statement(kind, id, expr_ast, out)
            }
            AdsStmtAst::Exit => {
                out.push(AdsInstruction::Exit);
                Ok(())
            }
            AdsStmtAst::If(pred_ast, then_ast, else_ast) => {
                self.typecheck_if_statement(pred_ast, then_ast, else_ast, out)
            }
            AdsStmtAst::Print(expr_ast) => {
                self.typecheck_print_statement(expr_ast, out)
            }
            AdsStmtAst::Relax => Ok(()),
            AdsStmtAst::RunUntil(breakpoint_ast) => {
                self.typecheck_run_until_statement(breakpoint_ast, out)
            }
            AdsStmtAst::Set(lvalue, expr_ast) => {
                self.typecheck_set_statement(lvalue, expr_ast, out)
            }
            AdsStmtAst::Step => {
                out.push(AdsInstruction::Step);
                Ok(())
            }
            AdsStmtAst::Use(expr_ast) => {
                self.typecheck_use_statement(expr_ast, out)
            }
            AdsStmtAst::When(breakpoint_ast, do_ast) => {
                self.typecheck_when_statement(breakpoint_ast, do_ast, out)
            }
            AdsStmtAst::While(pred_ast, do_ast) => {
                self.typecheck_while_statement(pred_ast, do_ast, out)
            }
        }
    }

    fn typecheck_declare_statement(
        &mut self,
        kind: DeclareAst,
        id: IdentifierAst,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let (expr_type, static_value) =
            errs.with(self.typecheck_expr(expr_ast, out));
        let kind = match kind {
            DeclareAst::Let => AdsDeclKind::Constant(static_value),
            DeclareAst::Var => AdsDeclKind::Variable,
        };
        self.env.add_declaration(kind, id, expr_type);
        errs.result()
    }

    fn typecheck_if_statement(
        &mut self,
        pred_ast: ExprAst,
        then_ast: Vec<AdsStmtAst>,
        else_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let _static_pred = errs.with(self.typecheck_predicate(pred_ast, out));
        // TODO: use static_pred to elide predicate and dead branch
        self.env.push_scope();
        let mut then_stmts = Vec::<AdsInstruction>::new();
        errs.also(self.typecheck_statements(then_ast, &mut then_stmts));
        self.env.pop_scope(&mut then_stmts);
        self.env.push_scope();
        let mut else_stmts = Vec::<AdsInstruction>::new();
        errs.also(self.typecheck_statements(else_ast, &mut else_stmts));
        self.env.pop_scope(&mut else_stmts);
        if !else_stmts.is_empty() {
            then_stmts.push(AdsInstruction::Jump(else_stmts.len() as isize));
        }
        out.push(AdsInstruction::BranchUnless(then_stmts.len() as isize));
        out.append(&mut then_stmts);
        out.append(&mut else_stmts);
        errs.result()
    }

    fn typecheck_print_statement(
        &mut self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let (_, errs) = self.typecheck_expr(expr_ast, out);
        errs.result()?;
        out.push(AdsInstruction::Print);
        Ok(())
    }

    fn typecheck_run_until_statement(
        &mut self,
        breakpoint_ast: BreakpointAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut breakpoint_ops = Vec::<AdsInstruction>::new();
        let breakpoint_kind =
            self.typecheck_breakpoint(breakpoint_ast, &mut breakpoint_ops)?;
        let (outer_ref, inner_ref) = if self.env.in_global_frame() {
            (AdsFrameRef::Global, AdsFrameRef::Global)
        } else {
            (AdsFrameRef::Local(0), AdsFrameRef::Local(1))
        };
        let index = self.env.frame_end();
        out.push(AdsInstruction::PushValue(ExprValue::Boolean(false)));
        out.append(&mut breakpoint_ops);
        out.push(AdsInstruction::PushHandler(breakpoint_kind, 1));
        out.push(AdsInstruction::Jump(3));
        out.push(AdsInstruction::PushValue(ExprValue::Boolean(true)));
        out.push(AdsInstruction::SetValue(inner_ref, index));
        out.push(AdsInstruction::Return);
        out.push(AdsInstruction::Step);
        out.push(AdsInstruction::GetValue(outer_ref, index));
        out.push(AdsInstruction::BranchUnless(-3));
        out.push(AdsInstruction::PopHandler);
        out.push(AdsInstruction::PopValue);
        Ok(())
    }

    fn typecheck_set_statement(
        &mut self,
        lvalue_ast: LValueAst,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let lvalue_span = lvalue_ast.span;
        let expr_span = expr_ast.span;
        let (expr_type, _) = errs.with(self.typecheck_expr(expr_ast, out));
        let lvalue_type = errs.with(self.typecheck_lvalue(lvalue_ast, out));
        if !(expr_type == ExprType::Bottom
            || lvalue_type == ExprType::Bottom
            || expr_type == lvalue_type)
        {
            errs.push(AdsError::VariableTypeError {
                expr_span,
                expr_type,
                lvalue_span,
                lvalue_type,
            });
        }
        errs.result()
    }

    fn typecheck_use_statement(
        &mut self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        match errs.ok(self.env.typecheck_expression(expr_ast)) {
            Some((_, ExprType::String, Some(path_value))) => {
                let path = self.joined_path(path_value.unwrap_str_ref());
                match self.cache.fetch_or_get_cached_utf8(&path) {
                    Ok(source_code) => {
                        if let Some(ast) = errs
                            .ok(AdsModuleAst::parse_source(source_code)
                                .map_err(Errs::coerce))
                        {
                            errs.also(
                                self.typecheck_statements(ast.statements, out),
                            );
                        }
                    }
                    Err(error) => {
                        errs.push(AdsError::SrcCacheError {
                            path,
                            path_span: expr_span,
                            error,
                        });
                    }
                }
            }
            Some((_, ExprType::String, None)) => {
                errs.push(AdsError::PathNotStatic { expr_span });
            }
            Some((_, expr_type, _)) => {
                errs.push(AdsError::PathTypeError { expr_span, expr_type });
            }
            None => {}
        }
        errs.result()
    }

    fn typecheck_when_statement(
        &mut self,
        breakpoint_ast: BreakpointAst,
        do_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        if let Some(kind) =
            errs.ok(self.typecheck_breakpoint(breakpoint_ast, out))
        {
            out.push(AdsInstruction::PushHandler(kind, 1));
        }
        self.env.push_frame();
        let mut do_stmts = Vec::<AdsInstruction>::new();
        errs.also(self.typecheck_statements(do_ast, &mut do_stmts));
        self.env.pop_frame(&mut do_stmts);
        do_stmts.push(AdsInstruction::Return);
        self.env.add_handler();
        out.push(AdsInstruction::Jump(do_stmts.len() as isize));
        out.append(&mut do_stmts);
        errs.result()
    }

    fn typecheck_while_statement(
        &mut self,
        pred_ast: ExprAst,
        do_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let mut pred_insts = Vec::<AdsInstruction>::new();
        let static_pred =
            errs.with(self.typecheck_predicate(pred_ast, &mut pred_insts));
        self.env.push_scope();
        let mut do_stmts = Vec::<AdsInstruction>::new();
        errs.also(self.typecheck_statements(do_ast, &mut do_stmts));
        self.env.pop_scope(&mut do_stmts);
        let do_stmts_len = do_stmts.len() as isize;
        match static_pred {
            Some(false) => {}
            Some(true) => {
                out.append(&mut do_stmts);
                out.push(AdsInstruction::Jump(-1 - do_stmts_len));
            }
            None => {
                let pred_insts_len = pred_insts.len() as isize;
                out.push(AdsInstruction::Jump(do_stmts_len));
                out.append(&mut do_stmts);
                out.append(&mut pred_insts);
                out.push(AdsInstruction::BranchIf(
                    -1 - do_stmts_len - pred_insts_len,
                ));
            }
        }
        errs.result()
    }

    fn typecheck_expr(
        &self,
        ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> ((ExprType, Option<ExprValue>), Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        match errs.ok(self.env.typecheck_expression(ast)) {
            Some((mut ops, expr_type, static_value)) => {
                out.append(&mut ops);
                ((expr_type, static_value), errs)
            }
            None => ((ExprType::Bottom, None), errs),
        }
    }

    fn typecheck_predicate(
        &self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> (Option<bool>, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        let (expr_type, static_value) =
            errs.with(self.typecheck_expr(expr_ast, out));
        let static_pred = if let ExprType::Boolean = expr_type {
            static_value.as_ref().map(ExprValue::unwrap_bool)
        } else {
            errs.push(AdsError::PredicateTypeError { expr_span, expr_type });
            None
        };
        (static_pred, errs)
    }

    fn typecheck_breakpoint(
        &self,
        ast: BreakpointAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<WatchKind> {
        let kind = match ast {
            BreakpointAst::Pc(expr_ast) => {
                self.typecheck_breakpoint_addr(expr_ast, out)?;
                WatchKind::Pc
            }
            BreakpointAst::Read(expr_ast) => {
                self.typecheck_breakpoint_addr(expr_ast, out)?;
                WatchKind::Read
            }
            BreakpointAst::Write(expr_ast) => {
                self.typecheck_breakpoint_addr(expr_ast, out)?;
                WatchKind::Write
            }
        };
        Ok(kind)
    }

    fn typecheck_breakpoint_addr(
        &self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        match errs.with(self.typecheck_expr(expr_ast, out)) {
            (ExprType::Integer, _) => {}
            // TODO: Allow `ExprType::Label` as well.
            (expr_type, _) => errs
                .push(AdsError::MemoryAddrTypeError { expr_span, expr_type }),
        }
        errs.result()
    }

    fn typecheck_lvalue(
        &self,
        lvalue_ast: LValueAst,
        out: &mut Vec<AdsInstruction>,
    ) -> (ExprType, Errs<AdsError>) {
        match lvalue_ast.node {
            LValueAstNode::Memory(expr_ast) => {
                self.typecheck_memory_lvalue(expr_ast, out)
            }
            LValueAstNode::Tuple(lvalue_asts) => {
                self.typecheck_tuple_lvalue(lvalue_asts, out)
            }
            LValueAstNode::Variable(name) => {
                self.typecheck_variable_lvalue(lvalue_ast.span, name, out)
            }
        }
    }

    fn typecheck_memory_lvalue(
        &self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> (ExprType, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        let (expr_type, _) = errs.with(self.typecheck_expr(expr_ast, out));
        out.push(AdsInstruction::SetMemory);
        // TODO: Allow `ExprType::Label` as well.
        if expr_type != ExprType::Integer {
            errs.push(AdsError::MemoryAddrTypeError { expr_span, expr_type });
        }
        (ExprType::Integer, errs)
    }

    fn typecheck_tuple_lvalue(
        &self,
        lvalue_asts: Vec<LValueAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> (ExprType, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        out.push(AdsInstruction::ExpandTuple);
        let mut types = Vec::<ExprType>::new();
        for lvalue_ast in lvalue_asts.into_iter().rev() {
            types.push(errs.with(self.typecheck_lvalue(lvalue_ast, out)));
        }
        types.reverse();
        (ExprType::Tuple(Rc::from(types)), errs)
    }

    fn typecheck_variable_lvalue(
        &self,
        id_span: SrcSpan,
        id_name: Rc<str>,
        out: &mut Vec<AdsInstruction>,
    ) -> (ExprType, Errs<AdsError>) {
        match self.env.get_declaration(&id_name) {
            Some((
                frame_ref,
                decl @ AdsDecl { kind: AdsDeclKind::Variable, .. },
            )) => {
                out.push(AdsInstruction::SetValue(
                    frame_ref,
                    decl.stack_index,
                ));
                (decl.var_type.clone(), Errs::new())
            }
            Some((
                _,
                decl @ AdsDecl { kind: AdsDeclKind::Constant(_), .. },
            )) => {
                let error = AdsError::CannotModifyConstant {
                    name: id_name,
                    lvalue_span: id_span,
                    decl_span: decl.id_span,
                };
                (decl.var_type.clone(), Errs::one(error))
            }
            None => {
                for &reg in self.env.register_names() {
                    if id_name.eq_ignore_ascii_case(reg) {
                        out.push(AdsInstruction::SetRegister(reg));
                        return (ExprType::Integer, Errs::new());
                    }
                }
                if id_name.eq_ignore_ascii_case("PC") {
                    out.push(AdsInstruction::SetPc);
                    return (ExprType::Integer, Errs::new());
                }
                let error =
                    AdsError::UnknownVariable { name: id_name, span: id_span };
                (ExprType::Bottom, Errs::one(error))
            }
        }
    }

    /// Given a relative path appearing in this assembly source file (e.g. in a
    /// `.BINARY` directive), join that path to this source file's parent
    /// directory.
    fn joined_path(&self, relative_path: &Rc<str>) -> Rc<str> {
        match AsRef::<Path>::as_ref(&*self.src_path).parent() {
            None => relative_path.clone(),
            Some(base_path) => {
                let joined = base_path.join(&**relative_path);
                // We can safely `unwrap()` the `to_str()` here because
                // `joined` was made from `Path`s that came from `str`s.
                Rc::<str>::from(joined.to_str().unwrap())
            }
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsFrameRef, AdsInstruction, AdsProgram, ExprValue, SimEnv};
    use crate::bus::{WatchKind, new_open_bus};
    use crate::error::StrSrcCache;
    use crate::expr::ExprBinOp;
    use crate::proc::Mos6502;
    use num_bigint::BigInt;
    use std::rc::Rc;

    fn compile(source: &str) -> Vec<AdsInstruction> {
        let mut cache = StrSrcCache::new();
        let path = Rc::<str>::from("input");
        let bus = new_open_bus(16);
        let cpu = Mos6502::new();
        let sim = SimEnv::new(vec![("cpu".to_string(), (Box::new(cpu), bus))]);
        AdsProgram::compile_source(&mut cache, path, source, &sim)
            .unwrap()
            .instructions
    }

    fn int_value(value: i32) -> ExprValue {
        ExprValue::Integer(BigInt::from(value))
    }

    #[test]
    fn empty_program() {
        assert_eq!(compile(""), vec![AdsInstruction::Exit]);
    }

    #[test]
    fn exit_statement_only() {
        assert_eq!(compile("exit\n"), vec![AdsInstruction::Exit]);
    }

    #[test]
    fn if_statement() {
        assert_eq!(
            compile("if %false {\nstep\n}\n"),
            vec![
                AdsInstruction::PushValue(ExprValue::Boolean(false)),
                AdsInstruction::BranchUnless(1),
                AdsInstruction::Step,
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn if_else_statement() {
        assert_eq!(
            compile("if %false {\nprint 1\n} else {\nprint 2\n}\n"),
            vec![
                AdsInstruction::PushValue(ExprValue::Boolean(false)),
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
    fn let_statement() {
        assert_eq!(
            compile("let x = 5\n"),
            vec![
                AdsInstruction::PushValue(int_value(5)),
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn let_statement_with_pc() {
        assert_eq!(
            compile("let x = pc\n"),
            vec![AdsInstruction::GetPc, AdsInstruction::Exit]
        );
    }

    #[test]
    fn print_statement() {
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
    fn relax_statement() {
        assert_eq!(compile("relax\n"), vec![AdsInstruction::Exit]);
    }

    #[test]
    fn set_statement() {
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
    fn step_statement() {
        assert_eq!(
            compile("step\n"),
            vec![AdsInstruction::Step, AdsInstruction::Exit]
        );
    }

    #[test]
    fn when_statement() {
        assert_eq!(
            compile("when at $ff {\nprint 1\n}\nstep\n"),
            vec![
                AdsInstruction::PushValue(int_value(255)),
                AdsInstruction::PushHandler(WatchKind::Pc, 1),
                AdsInstruction::Jump(3),
                AdsInstruction::PushValue(int_value(1)),
                AdsInstruction::Print,
                AdsInstruction::Return,
                AdsInstruction::Step,
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn while_false_statement() {
        assert_eq!(
            compile("while %false {\nstep\n}\n"),
            vec![AdsInstruction::Exit]
        );
    }

    #[test]
    fn while_true_statement() {
        assert_eq!(
            compile("while %true {\nstep\n}\n"),
            vec![
                AdsInstruction::Step,
                AdsInstruction::Jump(-2),
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn while_statement() {
        assert_eq!(
            compile("while pc < 5 {\nstep\n}\n"),
            vec![
                AdsInstruction::Jump(1),
                AdsInstruction::Step,
                AdsInstruction::GetPc,
                AdsInstruction::PushValue(int_value(5)),
                AdsInstruction::BinOp(ExprBinOp::AnyCmpLt),
                AdsInstruction::BranchIf(-5),
                AdsInstruction::Exit,
            ]
        );
    }
}

//===========================================================================//
