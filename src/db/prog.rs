use super::env::{AdsDeclKind, AdsTypeEnv, AdsVariableImmutableReason};
use super::error::{
    AdsError, AdsResult, AdsSrcContext, AdsSrcLoc, AdsSrcParent,
};
use super::inst::{AdsFrameRef, AdsInstruction};
use crate::bus::WatchKind;
use crate::error::{Errs, SrcCache, SrcSpan};
use crate::expr::{
    ExprBinOp, ExprNotStaticReason, ExprStatic, ExprType, ExprTypeError,
    ExprUnOp, ExprValue,
};
use crate::parse::{
    AdsStmtAst, AssignOpAst, AssignOpKind, BreakpointAst, CompoundIdAst,
    DeclarationKind, ExprAst, IdentifierAst, LValueAst, LValueAstNode,
    SymbolTable,
};
use crate::system::SimSystem;
use num_bigint::BigInt;
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
        system: &SimSystem,
        symbols: &SymbolTable,
    ) -> AdsResult<AdsProgram> {
        let mut compiler = AdsCompiler::new(cache, src_path, system, symbols);
        let module = compiler.env.parse_source(source_code)?;
        let mut instructions = Vec::<AdsInstruction>::new();
        compiler.compile_statements(module.statements, &mut instructions)?;
        instructions.push(AdsInstruction::Exit);
        Ok(AdsProgram { instructions })
    }
}

//===========================================================================//

struct AdsCompiler<'a> {
    cache: &'a mut dyn SrcCache,
    env: AdsTypeEnv<'a>,
}

impl<'a> AdsCompiler<'a> {
    fn new(
        cache: &'a mut dyn SrcCache,
        root_path: Rc<str>,
        system: &'a SimSystem,
        symbols: &SymbolTable,
    ) -> AdsCompiler<'a> {
        AdsCompiler { cache, env: AdsTypeEnv::new(root_path, system, symbols) }
    }

    fn compile_statements(
        &mut self,
        statements: Vec<AdsStmtAst>,
        instructions_out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        for statement in statements {
            errs.also(self.compile_statement(statement, instructions_out));
        }
        errs.result()
    }

    fn compile_statement(
        &mut self,
        statement: AdsStmtAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        match statement {
            AdsStmtAst::Assert(pred_ast, message_ast) => {
                self.compile_assert_statement(pred_ast, message_ast, out)
            }
            AdsStmtAst::Declare(kind, id, expr_ast) => {
                self.compile_declare_statement(kind, id, expr_ast, out)
            }
            AdsStmtAst::Error(expr_ast) => {
                self.compile_error_statement(expr_ast, out)
            }
            AdsStmtAst::Exit => {
                out.push(AdsInstruction::Exit);
                Ok(())
            }
            AdsStmtAst::For(opt_id, expr_ast, do_ast) => {
                self.compile_for_statement(opt_id, expr_ast, do_ast, out)
            }
            AdsStmtAst::If(pred_ast, then_ast, else_ast) => {
                self.compile_if_statement(pred_ast, then_ast, else_ast, out)
            }
            AdsStmtAst::Print(expr_ast) => {
                self.compile_print_statement(expr_ast, out)
            }
            AdsStmtAst::Relax => Ok(()),
            AdsStmtAst::Run => {
                out.push(AdsInstruction::Step);
                out.push(AdsInstruction::Jump(-2));
                Ok(())
            }
            AdsStmtAst::RunUntil(breakpoint_ast) => {
                self.compile_run_until_statement(breakpoint_ast, out)
            }
            AdsStmtAst::Set(lvalue, op, expr_ast) => {
                self.compile_set_statement(lvalue, op, expr_ast, out)
            }
            AdsStmtAst::Step => {
                out.push(AdsInstruction::Step);
                Ok(())
            }
            AdsStmtAst::Use(expr_ast) => {
                self.compile_use_statement(expr_ast, out)
            }
            AdsStmtAst::When(breakpoint_ast, do_ast) => {
                self.compile_when_statement(breakpoint_ast, do_ast, out)
            }
            AdsStmtAst::While(pred_ast, do_ast) => {
                self.compile_while_statement(pred_ast, do_ast, out)
            }
            AdsStmtAst::With(proc_ast, do_ast) => {
                self.compile_with_statement(proc_ast, do_ast, out)
            }
        }
    }

    fn compile_assert_statement(
        &mut self,
        pred_ast: ExprAst,
        opt_message_ast: Option<ExprAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let pred_span = pred_ast.span;
        let mut pred_insts = Vec::<AdsInstruction>::new();
        let static_pred =
            errs.with(self.compile_predicate(pred_ast, &mut pred_insts));
        let mut message_insts = Vec::<AdsInstruction>::new();
        if let Some(msg_ast) = opt_message_ast {
            errs.also(self.compile_error_message(msg_ast, &mut message_insts));
        } else {
            let value = ExprValue::String(Rc::from("assertion failed"));
            message_insts.push(AdsInstruction::PushValue(value));
        }
        if static_pred.is_none() {
            out.append(&mut pred_insts);
            out.push(AdsInstruction::BranchIf(
                (message_insts.len() + 1) as isize,
            ));
        }
        if static_pred != Some(true) {
            out.append(&mut message_insts);
            out.push(AdsInstruction::Error { loc: self.make_loc(pred_span) });
        }
        errs.result()
    }

    fn compile_declare_statement(
        &mut self,
        kind: DeclarationKind,
        id: IdentifierAst,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let (expr_type, expr_static) =
            errs.with(self.compile_expr(expr_ast, out));
        let kind = match kind {
            DeclarationKind::Let => AdsDeclKind::Immutable(expr_static),
            DeclarationKind::Var => AdsDeclKind::Mutable,
        };
        self.env.add_declaration(kind, id, expr_type);
        errs.result()
    }

    fn compile_error_statement(
        &mut self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let loc = self.make_loc(expr_ast.span);
        self.compile_error_message(expr_ast, out)?;
        out.push(AdsInstruction::Error { loc });
        Ok(())
    }

    fn compile_for_statement(
        &mut self,
        opt_id: Option<IdentifierAst>,
        expr_ast: ExprAst,
        do_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        self.env.push_scope();
        // Set up sequence to iterate over:
        let seq_var = self.env.add_internal_variable();
        let expr_span = expr_ast.span;
        let var_type = match errs.with(self.compile_expr(expr_ast, out)).0 {
            ExprType::List(item_type) => Rc::unwrap_or_clone(item_type),
            ExprType::Bottom | ExprType::Undefined => ExprType::Undefined,
            expr_type => {
                errs.push(AdsError::ExprTypeError {
                    context: self.env.current_src_context(),
                    error: ExprTypeError::CannotUseTypeAsIterator {
                        expr_span,
                        expr_type,
                    },
                });
                ExprType::Undefined
            }
        };
        // Set up index of iteration:
        let index_var = self.env.add_internal_variable();
        out.push(AdsInstruction::PushValue(ExprValue::ZERO));
        // Compile loop body:
        let mut do_stmts = Vec::<AdsInstruction>::new();
        self.env.push_scope();
        if let Some(id) = opt_id {
            let reason = ExprNotStaticReason::Variable {
                span: id.span,
                name: id.name.clone(),
            };
            let decl_kind = AdsDeclKind::Immutable(Err(reason));
            self.env.add_declaration(decl_kind, id, var_type);
            do_stmts.push(AdsInstruction::GetValue(AdsFrameRef(0), seq_var));
            do_stmts.push(AdsInstruction::GetValue(AdsFrameRef(0), index_var));
            do_stmts.push(AdsInstruction::GetListIndex {
                context: self.env.current_src_context(),
                list_span: SrcSpan::INTERNAL,
                index_span: SrcSpan::INTERNAL,
            });
        }
        errs.also(self.compile_statements(do_ast, &mut do_stmts));
        self.env.pop_scope(&mut do_stmts);
        let do_stmts_len = do_stmts.len() as isize;
        // Start by jumping over the loop body and the index increment:
        out.push(AdsInstruction::Jump(do_stmts_len + 4));
        out.append(&mut do_stmts);
        // Add instructions to increment index of iteration:
        out.push(AdsInstruction::GetValue(AdsFrameRef(0), index_var));
        out.push(AdsInstruction::PushValue(ExprValue::ONE));
        out.push(AdsInstruction::BinOp {
            context: self.env.current_src_context(),
            binop: ExprBinOp::Add,
            op_span: SrcSpan::INTERNAL,
            lhs_span: SrcSpan::INTERNAL,
            rhs_span: SrcSpan::INTERNAL,
        });
        out.push(AdsInstruction::SetValue(AdsFrameRef(0), index_var));
        // Add instructions to check index against sequence length:
        out.push(AdsInstruction::GetValue(AdsFrameRef(0), index_var));
        out.push(AdsInstruction::GetValue(AdsFrameRef(0), seq_var));
        out.push(AdsInstruction::UnOp {
            context: self.env.current_src_context(),
            unop: ExprUnOp::Length,
            op_span: SrcSpan::INTERNAL,
            arg_span: SrcSpan::INTERNAL,
        });
        out.push(AdsInstruction::BinOp {
            context: self.env.current_src_context(),
            binop: ExprBinOp::CmpLt,
            op_span: SrcSpan::INTERNAL,
            lhs_span: SrcSpan::INTERNAL,
            rhs_span: SrcSpan::INTERNAL,
        });
        out.push(AdsInstruction::BranchIf(-9 - do_stmts_len));
        self.env.pop_scope(out);
        errs.result()
    }

    fn compile_if_statement(
        &mut self,
        pred_ast: ExprAst,
        then_ast: Vec<AdsStmtAst>,
        else_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let mut pred_insts = Vec::<AdsInstruction>::new();
        let static_pred =
            errs.with(self.compile_predicate(pred_ast, &mut pred_insts));
        let mut then_stmts = Vec::<AdsInstruction>::new();
        self.env.push_scope();
        errs.also(self.compile_statements(then_ast, &mut then_stmts));
        self.env.pop_scope(&mut then_stmts);
        let mut else_stmts = Vec::<AdsInstruction>::new();
        self.env.push_scope();
        errs.also(self.compile_statements(else_ast, &mut else_stmts));
        self.env.pop_scope(&mut else_stmts);
        match static_pred {
            None => {
                out.append(&mut pred_insts);
                if !else_stmts.is_empty() {
                    let else_stmts_len = else_stmts.len() as isize;
                    then_stmts.push(AdsInstruction::Jump(else_stmts_len));
                }
                let then_stmts_len = then_stmts.len() as isize;
                out.push(AdsInstruction::BranchUnless(then_stmts_len));
                out.append(&mut then_stmts);
                out.append(&mut else_stmts);
            }
            Some(true) => out.append(&mut then_stmts),
            Some(false) => out.append(&mut else_stmts),
        }
        errs.result()
    }

    fn compile_print_statement(
        &mut self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let expr_span = expr_ast.span;
        let (_, errs) = self.compile_expr(expr_ast, out);
        errs.result()?;
        out.push(AdsInstruction::Print { loc: self.make_loc(expr_span) });
        Ok(())
    }

    fn compile_run_until_statement(
        &mut self,
        breakpoint_ast: BreakpointAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        self.env.push_scope();
        let is_done_var = self.env.add_internal_variable();
        out.push(AdsInstruction::PushValue(ExprValue::FALSE));
        let breakpoint_kind = self.compile_breakpoint(breakpoint_ast, out)?;
        self.env.add_handler();
        out.push(AdsInstruction::PushHandler(breakpoint_kind, 1));
        out.push(AdsInstruction::Jump(3));
        out.push(AdsInstruction::PushValue(ExprValue::TRUE));
        out.push(AdsInstruction::SetValue(AdsFrameRef(1), is_done_var));
        out.push(AdsInstruction::Return);
        out.push(AdsInstruction::Step);
        out.push(AdsInstruction::GetValue(AdsFrameRef(0), is_done_var));
        out.push(AdsInstruction::BranchUnless(-3));
        self.env.pop_scope(out);
        Ok(())
    }

    fn compile_set_statement(
        &mut self,
        lvalue_ast: LValueAst,
        op_ast: AssignOpAst,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        self.env.push_scope();
        let lvalue_span = lvalue_ast.span;
        let mut read_insts = Vec::<AdsInstruction>::new();
        let mut write_insts = Vec::<AdsInstruction>::new();
        let lvalue_type = errs.with(self.compile_lvalue(
            lvalue_ast,
            &mut LValueSink {
                setup_out: out,
                read_out: match op_ast.kind {
                    AssignOpKind::Equals => LValueRead::None,
                    AssignOpKind::OpEquals(_) => {
                        LValueRead::Copy(&mut read_insts)
                    }
                },
                write_out: &mut write_insts,
            },
        ));
        out.extend(read_insts);
        let expr_span = expr_ast.span;
        let (expr_type, _) = errs.with(self.compile_expr(expr_ast, out));
        errs.also(self.compile_assign_op(
            lvalue_span,
            lvalue_type,
            op_ast,
            expr_span,
            expr_type,
            out,
        ));
        out.extend(write_insts);
        self.env.pop_scope(out);
        errs.result()
    }

    fn compile_use_statement(
        &mut self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        match errs.ok(self.env.typecheck_expression(expr_ast)) {
            None | Some((_, ExprType::Undefined, _)) => {}
            Some((_, ExprType::String, Ok(path_value))) => {
                let path = self.joined_path(path_value.unwrap_str_ref());
                match self.cache.fetch_or_get_cached_utf8(&path) {
                    Ok(source_code) => {
                        let context = Rc::new(AdsSrcContext {
                            path,
                            parent: AdsSrcParent::Use(AdsSrcLoc {
                                span: expr_span,
                                context: self.env.current_src_context(),
                            }),
                        });
                        // TODO: Isolate the environment somehow here;
                        // variables from the parent source file should not
                        // be visible to the child source file, nor
                        // vice-versa; only handlers declared in the child
                        // source file should affect the parent source
                        // file.
                        self.env.push_src_context(context);
                        if let Some(module) =
                            errs.ok(self.env.parse_source(source_code))
                        {
                            errs.also(
                                self.compile_statements(
                                    module.statements,
                                    out,
                                ),
                            )
                        }
                        self.env.pop_src_context();
                    }
                    Err(error) => {
                        errs.push(AdsError::SrcCacheError {
                            path,
                            path_loc: self.make_loc(expr_span),
                            error,
                        });
                    }
                }
            }
            Some((_, ExprType::String | ExprType::Bottom, Err(reason))) => {
                errs.push(AdsError::PathNotStatic {
                    expr_loc: self.make_loc(expr_span),
                    reason,
                });
            }
            Some((_, expr_type, _)) => {
                errs.push(AdsError::PathTypeError {
                    expr_loc: self.make_loc(expr_span),
                    expr_type,
                });
            }
        }
        errs.result()
    }

    fn compile_when_statement(
        &mut self,
        breakpoint_ast: BreakpointAst,
        do_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        if let Some(kind) =
            errs.ok(self.compile_breakpoint(breakpoint_ast, out))
        {
            out.push(AdsInstruction::PushHandler(kind, 1));
        }
        self.env.push_frame();
        let mut do_stmts = Vec::<AdsInstruction>::new();
        errs.also(self.compile_statements(do_ast, &mut do_stmts));
        self.env.pop_frame(&mut do_stmts);
        do_stmts.push(AdsInstruction::Return);
        self.env.add_handler();
        out.push(AdsInstruction::Jump(do_stmts.len() as isize));
        out.append(&mut do_stmts);
        errs.result()
    }

    fn compile_while_statement(
        &mut self,
        pred_ast: ExprAst,
        do_ast: Vec<AdsStmtAst>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let mut pred_insts = Vec::<AdsInstruction>::new();
        let static_pred =
            errs.with(self.compile_predicate(pred_ast, &mut pred_insts));
        self.env.push_scope();
        let mut do_stmts = Vec::<AdsInstruction>::new();
        errs.also(self.compile_statements(do_ast, &mut do_stmts));
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

    fn compile_with_statement(
        &mut self,
        expr_ast: ExprAst,
        maybe_do_ast: Option<Vec<AdsStmtAst>>,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        match errs.ok(self.env.typecheck_expression(expr_ast)) {
            None | Some((_, ExprType::Undefined, _)) => {}
            Some((_, ExprType::String, Ok(proc_value))) => {
                let proc_name = proc_value.unwrap_str();
                if self.env.contains_processor(&proc_name) {
                    if let Some(do_ast) = maybe_do_ast {
                        self.env.push_scope();
                        self.env.set_proc(proc_name, out);
                        errs.also(self.compile_statements(do_ast, out));
                        self.env.pop_scope(out);
                    } else {
                        self.env.set_proc(proc_name, out);
                    }
                } else {
                    errs.push(AdsError::UnknownProc {
                        proc_name,
                        expr_loc: self.make_loc(expr_span),
                        valid_procs: self.env.processor_names(),
                    });
                }
            }
            Some((_, ExprType::String | ExprType::Bottom, Err(reason))) => {
                errs.push(AdsError::ProcNotStatic {
                    expr_loc: self.make_loc(expr_span),
                    reason,
                });
            }
            Some((_, expr_type, _)) => {
                errs.push(AdsError::ProcTypeError {
                    expr_loc: self.make_loc(expr_span),
                    expr_type,
                });
            }
        }
        errs.result()
    }

    fn compile_expr(
        &self,
        ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> ((ExprType, ExprStatic), Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        let type_static = match errs.ok(self.env.typecheck_expression(ast)) {
            Some((mut ops, expr_type, expr_static)) => {
                out.append(&mut ops);
                (expr_type, expr_static)
            }
            None => (ExprType::Undefined, Err(ExprNotStaticReason::TypeError)),
        };
        (type_static, errs)
    }

    fn compile_error_message(
        &self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        let (expr_type, _) = errs.with(self.compile_expr(expr_ast, out));
        if !expr_type.is_subtype_of(&ExprType::String) {
            errs.push(AdsError::ExprTypeError {
                context: self.env.current_src_context(),
                error: ExprTypeError::CannotUseTypeAsErrorMessage {
                    expr_span,
                    expr_type,
                },
            });
        }
        errs.result()
    }

    fn compile_predicate(
        &self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> (Option<bool>, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        let (expr_type, expr_static) =
            errs.with(self.compile_expr(expr_ast, out));
        let pred_static = if expr_type.is_subtype_of(&ExprType::Boolean) {
            expr_static.ok().as_ref().map(ExprValue::unwrap_bool)
        } else {
            errs.push(AdsError::ExprTypeError {
                context: self.env.current_src_context(),
                error: ExprTypeError::CannotUseTypeAsPredicate {
                    expr_span,
                    expr_type,
                },
            });
            None
        };
        (pred_static, errs)
    }

    fn compile_breakpoint(
        &self,
        ast: BreakpointAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<WatchKind> {
        let kind = match ast {
            BreakpointAst::Pc(expr_ast) => {
                self.compile_address(expr_ast, out)?;
                WatchKind::Pc
            }
            BreakpointAst::Read(expr_ast) => {
                self.compile_address(expr_ast, out)?;
                WatchKind::Read
            }
            BreakpointAst::Write(expr_ast) => {
                self.compile_address(expr_ast, out)?;
                WatchKind::Write
            }
        };
        Ok(kind)
    }

    fn compile_address(
        &self,
        expr_ast: ExprAst,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let expr_span = expr_ast.span;
        let expr_type = errs.with(self.compile_expr(expr_ast, out)).0;
        if !expr_type.is_subtype_of(&ExprType::Integer)
            && !expr_type.is_subtype_of(&ExprType::Label)
        {
            errs.push(AdsError::ExprTypeError {
                context: self.env.current_src_context(),
                error: ExprTypeError::CannotUseTypeAsMemoryAddress {
                    expr_span,
                    expr_type,
                },
            });
        }
        errs.result()
    }

    fn compile_assign_op(
        &self,
        lvalue_span: SrcSpan,
        lvalue_type: ExprType,
        op_ast: AssignOpAst,
        expr_span: SrcSpan,
        expr_type: ExprType,
        out: &mut Vec<AdsInstruction>,
    ) -> AdsResult<()> {
        let mut errs = Errs::<AdsError>::new();
        let (result_span, result_type) = match op_ast.kind {
            AssignOpKind::Equals => (expr_span, expr_type),
            AssignOpKind::OpEquals(binop_kind) => {
                let result_span = lvalue_span.merged_with(expr_span);
                let context = self.env.current_src_context();
                let result_type = match errs.ok(ExprBinOp::typecheck(
                    (op_ast.span, binop_kind),
                    lvalue_span,
                    lvalue_type.clone(),
                    expr_span,
                    expr_type,
                )
                .map_err(|errs| {
                    errs.map(|error| AdsError::ExprTypeError {
                        context: context.clone(),
                        error,
                    })
                })) {
                    Some((binop, result_type)) => {
                        out.push(AdsInstruction::BinOp {
                            context,
                            binop,
                            op_span: op_ast.span,
                            lhs_span: lvalue_span,
                            rhs_span: expr_span,
                        });
                        result_type
                    }
                    None => ExprType::Undefined,
                };
                (result_span, result_type)
            }
        };
        if !result_type.is_subtype_of(&lvalue_type) {
            errs.push(AdsError::VariableTypeError {
                expr_loc: self.make_loc(result_span),
                expr_type: result_type,
                lvalue_loc: self.make_loc(lvalue_span),
                lvalue_type,
            });
        }
        errs.result()
    }

    fn compile_lvalue(
        &mut self,
        lvalue_ast: LValueAst,
        sink: &mut LValueSink,
    ) -> (ExprType, Errs<AdsError>) {
        match lvalue_ast.node {
            LValueAstNode::Index(base_ast, bracket_span, index_ast) => self
                .compile_index_lvalue(
                    *base_ast,
                    bracket_span,
                    index_ast,
                    sink,
                ),
            LValueAstNode::Memory(expr_ast) => {
                self.compile_memory_lvalue(expr_ast, sink)
            }
            LValueAstNode::Tuple(item_asts) => {
                self.compile_tuple_lvalue(item_asts, sink)
            }
            LValueAstNode::Variable(compound) => {
                self.compile_variable_lvalue(compound, sink)
            }
            LValueAstNode::Wildcard => {
                self.compile_wildcard_lvalue(lvalue_ast.span, sink)
            }
        }
    }

    fn compile_memory_lvalue(
        &mut self,
        expr_ast: ExprAst,
        sink: &mut LValueSink,
    ) -> (ExprType, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        match &mut sink.read_out {
            LValueRead::Copy(read_out) | LValueRead::Take(read_out) => {
                let frame = AdsFrameRef(0);
                let addr_var = self.env.add_internal_variable();
                errs.also(self.compile_address(expr_ast, sink.setup_out));
                read_out.push(AdsInstruction::GetValue(frame, addr_var));
                read_out.push(AdsInstruction::GetMemory);
                sink.write_out.push(AdsInstruction::GetValue(frame, addr_var));
            }
            LValueRead::None => {
                errs.also(self.compile_address(expr_ast, sink.write_out));
            }
        }
        sink.write_out.push(AdsInstruction::SetMemory);
        (ExprType::Integer, errs)
    }

    fn compile_tuple_lvalue(
        &mut self,
        lvalue_asts: Vec<LValueAst>,
        sink: &mut LValueSink,
    ) -> (ExprType, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        sink.write_out.push(AdsInstruction::ExpandTupleRev);
        let num_items = lvalue_asts.len();
        let mut types = Vec::<ExprType>::with_capacity(num_items);
        for lvalue_ast in lvalue_asts {
            types.push(errs.with(self.compile_lvalue(lvalue_ast, sink)));
        }
        match &mut sink.read_out {
            LValueRead::Copy(read_out) | LValueRead::Take(read_out) => {
                read_out.push(AdsInstruction::MakeTuple(num_items));
            }
            LValueRead::None => {}
        }
        (ExprType::Tuple(Rc::from(types)), errs)
    }

    fn compile_index_lvalue(
        &mut self,
        seq_ast: LValueAst,
        bracket_span: SrcSpan,
        index_ast: ExprAst,
        sink: &mut LValueSink,
    ) -> (ExprType, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        // Compile sequence lvalue:
        let seq_span = seq_ast.span;
        let (seq_type, read_seq, write_seq) =
            errs.with(self.compile_index_lvalue_sequence(seq_ast, sink));
        // Compile index expression:
        let index_span = index_ast.span;
        let mut read_index = Vec::<AdsInstruction>::new();
        let (index_type, index_static) =
            errs.with(self.compile_expr(index_ast, &mut read_index));
        // Compile indexing operation:
        let item_type = match (seq_type, index_type) {
            (ExprType::Undefined, _) | (_, ExprType::Undefined) => {
                ExprType::Undefined
            }
            (ExprType::List(item_type), ExprType::Integer)
            | (ExprType::List(item_type), ExprType::Bottom) => {
                self.compile_index_lvalue_for_list(
                    seq_span, read_seq, write_seq, index_span, read_index,
                    sink,
                );
                Rc::unwrap_or_clone(item_type)
            }
            (ExprType::Tuple(item_types), ExprType::Integer)
            | (ExprType::Tuple(item_types), ExprType::Bottom) => {
                match index_static {
                    Ok(index) => {
                        let index = index.unwrap_int();
                        if index >= BigInt::ZERO
                            && index < BigInt::from(item_types.len())
                        {
                            let index = usize::try_from(index).unwrap();
                            self.compile_index_lvalue_for_tuple(
                                read_seq, write_seq, index, sink,
                            );
                            item_types[index].clone()
                        } else {
                            errs.push(AdsError::ExprTypeError {
                                context: self.env.current_src_context(),
                                error: ExprTypeError::TupleIndexOutOfRange {
                                    tuple_span: seq_span,
                                    item_types,
                                    index_span,
                                    index_value: index,
                                },
                            });
                            ExprType::Undefined
                        }
                    }
                    Err(reason) => {
                        errs.push(AdsError::ExprTypeError {
                            context: self.env.current_src_context(),
                            error: ExprTypeError::TupleIndexNotStatic {
                                index_span,
                                reason,
                            },
                        });
                        ExprType::Undefined
                    }
                }
            }
            (ExprType::List(_), index_type)
            | (ExprType::Tuple(_), index_type) => {
                errs.push(AdsError::ExprTypeError {
                    context: self.env.current_src_context(),
                    error: ExprTypeError::CannotUseTypeAsIndex {
                        index_span,
                        index_type,
                    },
                });
                ExprType::Undefined
            }
            (indexed_type, _) => {
                errs.push(AdsError::ExprTypeError {
                    context: self.env.current_src_context(),
                    error: ExprTypeError::CannotIndexIntoType {
                        bracket_span,
                        indexed_span: seq_span,
                        indexed_type,
                    },
                });
                ExprType::Undefined
            }
        };
        (item_type, errs)
    }

    fn compile_index_lvalue_sequence(
        &mut self,
        seq_ast: LValueAst,
        sink: &mut LValueSink,
    ) -> ((ExprType, Vec<AdsInstruction>, Vec<AdsInstruction>), Errs<AdsError>)
    {
        let mut errs = Errs::<AdsError>::new();
        let mut read_seq = Vec::<AdsInstruction>::new();
        let mut write_seq = Vec::<AdsInstruction>::new();
        let seq_type = {
            let mut sub = LValueSink {
                setup_out: sink.setup_out,
                read_out: match &mut sink.read_out {
                    LValueRead::Copy(_) => LValueRead::Copy(&mut read_seq),
                    LValueRead::Take(_) | LValueRead::None => {
                        LValueRead::Take(&mut read_seq)
                    }
                },
                write_out: &mut write_seq,
            };
            errs.with(self.compile_lvalue(seq_ast, &mut sub))
        };
        ((seq_type, read_seq, write_seq), errs)
    }

    fn compile_index_lvalue_for_list(
        &mut self,
        seq_span: SrcSpan,
        read_seq: Vec<AdsInstruction>,
        write_seq: Vec<AdsInstruction>,
        index_span: SrcSpan,
        read_index: Vec<AdsInstruction>,
        sink: &mut LValueSink,
    ) {
        match &mut sink.read_out {
            LValueRead::Copy(copy_out) => {
                let seq_var = self.env.add_internal_variable();
                sink.setup_out.extend(read_seq);
                let index_var = self.env.add_internal_variable();
                sink.setup_out.extend(read_index);
                let frame = AdsFrameRef(0);
                copy_out.push(AdsInstruction::GetValue(frame, seq_var));
                copy_out.push(AdsInstruction::GetValue(frame, index_var));
                copy_out.push(AdsInstruction::GetListIndex {
                    context: self.env.current_src_context(),
                    list_span: seq_span,
                    index_span,
                });
                sink.write_out.push(AdsInstruction::GetValue(frame, seq_var));
                sink.write_out
                    .push(AdsInstruction::GetValue(frame, index_var));
                // now stack is ...new_item, seq, index
            }
            LValueRead::Take(take_out) => {
                take_out.extend(read_seq);
                take_out.extend(read_index);
                // now stack is ...value, seq, index
                take_out.push(AdsInstruction::TakeListIndex {
                    context: self.env.current_src_context(),
                    list_span: seq_span,
                    index_span,
                });
                // now stack is ...value, old_item, seq, index
                take_out.push(AdsInstruction::RotateValues(4, 2));
                // now stack is ...seq, index, value, old_item
                // ...
                // now stack is ...seq, index, new_item
                sink.write_out.push(AdsInstruction::RotateValues(3, 1));
                // now stack is ...new_item, seq, index
            }
            LValueRead::None => {
                sink.write_out.extend(read_seq);
                sink.write_out.extend(read_index);
                // now stack is ...new_item, seq, index
            }
        }
        // now stack is ...new_item, seq, index
        sink.write_out.push(AdsInstruction::SetListIndex {
            context: self.env.current_src_context(),
            list_span: seq_span,
            index_span,
        });
        sink.write_out.extend(write_seq);
    }

    fn compile_index_lvalue_for_tuple(
        &mut self,
        read_seq: Vec<AdsInstruction>,
        write_seq: Vec<AdsInstruction>,
        index: usize,
        sink: &mut LValueSink,
    ) {
        match &mut sink.read_out {
            LValueRead::Copy(copy_out) => {
                let seq_var = self.env.add_internal_variable();
                sink.setup_out.extend(read_seq);
                let frame = AdsFrameRef(0);
                copy_out.push(AdsInstruction::GetValue(frame, seq_var));
                copy_out.push(AdsInstruction::GetTupleItem(index));
                sink.write_out.push(AdsInstruction::GetValue(frame, seq_var));
            }
            LValueRead::Take(take_out) => {
                take_out.extend(read_seq);
                // now stack is ...value, seq
                take_out.push(AdsInstruction::TakeTupleItem(index));
                // now stack is ...value, old_item, seq
                take_out.push(AdsInstruction::RotateValues(3, 1));
                // now stack is ...seq, value, old_item
                //...
                // now stack is ...seq, new_item
                sink.write_out.push(AdsInstruction::RotateValues(2, 1));
                // now stack is ...new_item, seq
            }
            LValueRead::None => {
                sink.write_out.extend(read_seq);
            }
        }
        sink.write_out.push(AdsInstruction::SetTupleItem(index));
        sink.write_out.extend(write_seq);
    }

    fn compile_variable_lvalue(
        &self,
        compound: CompoundIdAst,
        sink: &mut LValueSink,
    ) -> (ExprType, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        let Some(variable) = self.env.get_variable(&compound.name) else {
            errs.push(AdsError::UnknownVariable {
                name: compound.name,
                loc: self.make_loc(compound.span),
            });
            return (ExprType::Undefined, errs);
        };
        match &mut sink.read_out {
            LValueRead::Copy(copy_out) => {
                copy_out.push(variable.getter_instruction());
            }
            LValueRead::Take(take_out) => {
                take_out.push(variable.taker_instruction());
            }
            LValueRead::None => {}
        }
        match variable.setter_instruction() {
            Ok(inst) => sink.write_out.push(inst),
            Err(AdsVariableImmutableReason::Builtin) => {
                errs.push(AdsError::CannotMutateBuiltin {
                    name: compound.name,
                    lvalue_loc: self.make_loc(compound.span),
                });
            }
            Err(AdsVariableImmutableReason::DeclaredImmutable(decl_loc)) => {
                errs.push(AdsError::CannotMutateImmutableVariable {
                    name: compound.name,
                    lvalue_loc: self.make_loc(compound.span),
                    decl_loc,
                });
            }
            Err(AdsVariableImmutableReason::Symbol) => {
                errs.push(AdsError::CannotMutateSymbol {
                    name: compound.name,
                    lvalue_loc: self.make_loc(compound.span),
                });
            }
        }
        (variable.expr_type(), errs)
    }

    fn compile_wildcard_lvalue(
        &self,
        wildcard_span: SrcSpan,
        sink: &mut LValueSink,
    ) -> (ExprType, Errs<AdsError>) {
        let mut errs = Errs::<AdsError>::new();
        match &mut sink.read_out {
            LValueRead::Copy(_) | LValueRead::Take(_) => {
                errs.push(AdsError::ExprTypeError {
                    context: self.env.current_src_context(),
                    error: ExprTypeError::WildcardAsValue {
                        span: wildcard_span,
                    },
                });
            }
            LValueRead::None => {
                sink.write_out.push(AdsInstruction::PopValue);
            }
        }
        (ExprType::Undefined, errs)
    }

    fn make_loc(&self, span: SrcSpan) -> AdsSrcLoc {
        AdsSrcLoc { span, context: self.env.current_src_context() }
    }

    /// Given a relative path appearing in the current source context (e.g. in
    /// a `use` statement), join that path to the current source file's parent
    /// directory.
    fn joined_path(&self, relative_path: &Rc<str>) -> Rc<str> {
        let context = self.env.current_src_context();
        match AsRef::<Path>::as_ref(&*context.path).parent() {
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

struct LValueSink<'a> {
    /// The lvalue may optionally emit instructions here that should be
    /// executed during the initial setup phase for the `set` statement.  It is
    /// permissible here (and only here) to add internal variables to the
    /// current scope; these will be automatically cleaned up at the end of the
    /// `set` statement.
    setup_out: &'a mut Vec<AdsInstruction>,
    /// Depending on the mode here, the lvalue may need to emit instructions
    /// here to push its current value onto the stack.  This can make use of
    /// internal variables added during the setup phase.
    read_out: LValueRead<'a>,
    /// The lvalue should emit instructions here to store the topmost value on
    /// the value stack into the lvalue location.  This can make use of
    /// internal variables added during the setup phase.
    write_out: &'a mut Vec<AdsInstruction>,
}

enum LValueRead<'a> {
    /// The lvalue should emit instructions here to push a copy of its current
    /// value onto the stack.  These instructions will be executed during
    /// either the setup phase or the write phase.
    Copy(&'a mut Vec<AdsInstruction>),
    /// The lvalue should emit instructions here to temporarily replace its
    /// current value with a placeholder value, and push it onto the stack; it
    /// is permissible to also insert additional values below the original
    /// topmost element of the stack, as long as those will get cleaned up
    /// afterwards by the instructions in `write_out`.  These instructions will
    /// be executed during the write phase.
    Take(&'a mut Vec<AdsInstruction>),
    /// There is no need to retrieve the lvalue's current value.
    None,
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{
        AdsFrameRef, AdsInstruction, AdsProgram, ExprValue, SimSystem,
    };
    use crate::addr::BitWidth;
    use crate::bus::{WatchKind, new_open_bus};
    use crate::error::StrSrcCache;
    use crate::expr::ExprBinOp;
    use crate::parse::SymbolTable;
    use crate::proc::Mos6502;
    use num_bigint::BigInt;
    use std::assert_matches;
    use std::rc::Rc;

    fn compile(source: &str) -> Vec<AdsInstruction> {
        let mut cache = StrSrcCache::new();
        let path = Rc::<str>::from("input");
        let bus = new_open_bus(BitWidth::SIXTEEN);
        let cpu = Mos6502::new();
        let system = SimSystem::new(
            vec![(Rc::from("CPU"), (Box::new(cpu), bus))],
            Vec::new(),
        );
        let symbols = SymbolTable::default();
        AdsProgram::compile_source(&mut cache, path, source, &system, &symbols)
            .unwrap()
            .instructions
    }

    fn int_value(value: i32) -> ExprValue {
        ExprValue::Integer(BigInt::from(value))
    }

    #[test]
    fn empty_program() {
        assert_matches!(compile("").as_slice(), [AdsInstruction::Exit]);
    }

    #[test]
    fn if_false_statement() {
        assert_matches!(
            compile("if %false {\nprint 1\n}\n").as_slice(),
            [AdsInstruction::Exit]
        );
    }

    #[test]
    fn if_true_statement() {
        assert_matches!(compile("if %true {\nprint 1\n}\n").as_slice(), [
            AdsInstruction::PushValue(int_value_1),
            AdsInstruction::Print { .. },
            AdsInstruction::Exit,
        ] if *int_value_1 == int_value(1));
    }

    #[test]
    fn if_statement() {
        assert_matches!(compile("if pc == 1 {\nexit\n}\n").as_slice(), [
            AdsInstruction::GetPc,
            AdsInstruction::PushValue(int_value_1),
            AdsInstruction::BinOp { binop: ExprBinOp::CmpEq, .. },
            AdsInstruction::BranchUnless(1),
            AdsInstruction::Exit,
            AdsInstruction::Exit,
        ] if *int_value_1 == int_value(1));
    }

    #[test]
    fn if_false_else_statement() {
        let instructions =
            compile("if %false {\nprint 1\n} else {\nprint 2\n}\n");
        assert_matches!(instructions.as_slice(), [
            AdsInstruction::PushValue(int_value_2),
            AdsInstruction::Print { .. },
            AdsInstruction::Exit,
        ] if *int_value_2 == int_value(2));
    }

    #[test]
    fn if_true_else_statement() {
        let instructions =
            compile("if %true {\nprint 1\n} else {\nprint 2\n}\n");
        assert_matches!(instructions.as_slice(), [
            AdsInstruction::PushValue(int_value_1),
            AdsInstruction::Print { .. },
            AdsInstruction::Exit,
        ] if *int_value_1 == int_value(1));
    }

    #[test]
    fn if_else_statement() {
        let instructions =
            compile("var x = %true\nif x {\nprint 1\n} else {\nprint 2\n}\n");
        assert_matches!(instructions.as_slice(), [
            AdsInstruction::PushValue(ExprValue::Boolean(true)),
            AdsInstruction::GetValue(AdsFrameRef(0), 0),
            AdsInstruction::BranchUnless(3),
            AdsInstruction::PushValue(int_value_1),
            AdsInstruction::Print { .. },
            AdsInstruction::Jump(2),
            AdsInstruction::PushValue(int_value_2),
            AdsInstruction::Print { .. },
            AdsInstruction::Exit,
        ] if *int_value_1 == int_value(1) && *int_value_2 == int_value(2));
    }

    #[test]
    fn let_statement() {
        assert_matches!(compile("let x = 5\n").as_slice(), [
            AdsInstruction::PushValue(int_value_5),
            AdsInstruction::Exit,
        ] if *int_value_5 == int_value(5));
    }

    #[test]
    fn let_statement_with_pc() {
        assert_matches!(
            compile("let x = pc\n").as_slice(),
            [AdsInstruction::GetPc, AdsInstruction::Exit]
        );
    }

    #[test]
    fn print_statement() {
        assert_matches!(compile("print 42\n").as_slice(), [
            AdsInstruction::PushValue(int_value_42),
            AdsInstruction::Print { .. },
            AdsInstruction::Exit,
        ] if *int_value_42 == int_value(42));
    }

    #[test]
    fn relax_statement() {
        assert_matches!(compile("relax\n").as_slice(), [AdsInstruction::Exit]);
    }

    #[test]
    fn run_statement() {
        assert_matches!(
            compile("run\n").as_slice(),
            [
                AdsInstruction::Step,
                AdsInstruction::Jump(-2),
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn set_statement() {
        assert_matches!(compile("var x = 1\nset x = 2\n").as_slice(), [
            AdsInstruction::PushValue(int_value_1),
            AdsInstruction::PushValue(int_value_2),
            AdsInstruction::SetValue(AdsFrameRef(0), 0),
            AdsInstruction::Exit,
        ] if *int_value_1 == int_value(1) && *int_value_2 == int_value(2));
    }

    #[test]
    fn step_statement() {
        assert_matches!(
            compile("step\n").as_slice(),
            [AdsInstruction::Step, AdsInstruction::Exit]
        );
    }

    #[test]
    fn when_statement() {
        let instructions = compile("when at $ff {\nprint 1\n}\nstep\n");
        assert_matches!(instructions.as_slice(), [
            AdsInstruction::PushValue(int_value_255),
            AdsInstruction::PushHandler(WatchKind::Pc, 1),
            AdsInstruction::Jump(3),
            AdsInstruction::PushValue(int_value_1),
            AdsInstruction::Print { .. },
            AdsInstruction::Return,
            AdsInstruction::Step,
            AdsInstruction::Exit,
        ] if *int_value_1 == int_value(1) && *int_value_255 == int_value(255));
    }

    #[test]
    fn while_false_statement() {
        assert_matches!(
            compile("while %false {\nstep\n}\n").as_slice(),
            [AdsInstruction::Exit]
        );
    }

    #[test]
    fn while_true_statement() {
        assert_matches!(
            compile("while %true {\nstep\n}\n").as_slice(),
            [
                AdsInstruction::Step,
                AdsInstruction::Jump(-2),
                AdsInstruction::Exit,
            ]
        );
    }

    #[test]
    fn while_statement() {
        assert_matches!(compile("while pc < 5 {\nstep\n}\n").as_slice(), [
            AdsInstruction::Jump(1),
            AdsInstruction::Step,
            AdsInstruction::GetPc,
            AdsInstruction::PushValue(int_value_5),
            AdsInstruction::BinOp { binop: ExprBinOp::CmpLt, .. },
            AdsInstruction::BranchIf(-5),
            AdsInstruction::Exit,
        ] if *int_value_5 == int_value(5));
    }
}

//===========================================================================//
