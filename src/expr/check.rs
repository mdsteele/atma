use super::binop::ExprBinOp;
use super::env::{ExprEnv, ExprOp};
use super::error::{
    ExprEvalError, ExprNotStaticReason, ExprStatic, ExprTypeError,
    ExprTypeResult,
};
use super::unop::ExprUnOp;
use super::value::{ExprType, ExprValue};
use crate::error::{Errs, SrcSpan};
use crate::parse::{BinOpAst, ExprAst, ExprAstNode, UnOpAst};
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

const UNDEFINED: (ExprType, ExprStatic) =
    (ExprType::Undefined, Err(ExprNotStaticReason::TypeError));

//===========================================================================//

enum Task {
    Apply(SrcSpan, SrcSpan),
    BinOp((SrcSpan, BinOpAst), SrcSpan, SrcSpan),
    CondBranch(SrcSpan, ExprAst, ExprAst),
    CondElse(ExprNotStaticReason, usize, SrcSpan, ExprAst),
    CondJoin(usize),
    CondUnify(Result<bool, ExprNotStaticReason>, SrcSpan, SrcSpan),
    Expr(ExprAst),
    Index(SrcSpan, SrcSpan, SrcSpan),
    ListLiteral(Vec<SrcSpan>),
    LogBranch(bool, SrcSpan, SrcSpan, ExprAst),
    LogJoin(bool, usize),
    LogUnify(bool, SrcSpan, SrcSpan, SrcSpan),
    PhantomExpr(ExprAst),
    PhantomDone(usize),
    TupleLiteral(usize),
    UnOp((SrcSpan, UnOpAst), SrcSpan),
}

//===========================================================================//

pub(crate) struct ExprCompiler<'a, E: ExprEnv> {
    env: &'a E,
    types: Vec<(ExprType, ExprStatic)>,
    ops: Vec<E::Op>,
    tasks: Vec<Task>,
    errs: Errs<ExprTypeError>,
}

impl<'a, E: ExprEnv> ExprCompiler<'a, E> {
    pub(crate) fn new(env: &'a E) -> ExprCompiler<'a, E> {
        ExprCompiler {
            env,
            types: Vec::new(),
            ops: Vec::new(),
            tasks: Vec::new(),
            errs: Errs::new(),
        }
    }

    pub(crate) fn typecheck(
        mut self,
        expr: ExprAst,
    ) -> ExprTypeResult<(Vec<E::Op>, ExprType, ExprStatic)> {
        self.tasks.push(Task::Expr(expr));
        while let Some(task) = self.tasks.pop() {
            match task {
                Task::Apply(func_span, arg_span) => {
                    self.do_task_apply(func_span, arg_span);
                }
                Task::BinOp(binop_ast, lhs_span, rhs_span) => {
                    self.do_task_binop(binop_ast, lhs_span, rhs_span);
                }
                Task::CondBranch(pred_span, lhs_ast, rhs_ast) => {
                    self.do_task_cond_branch(pred_span, lhs_ast, rhs_ast);
                }
                Task::CondElse(reason, op_index, lhs_span, rhs_ast) => {
                    self.do_task_cond_else(
                        reason, op_index, lhs_span, rhs_ast,
                    );
                }
                Task::CondJoin(op_index) => self.do_task_cond_join(op_index),
                Task::CondUnify(pred_static, lhs_span, rhs_span) => {
                    self.do_task_cond_unify(pred_static, lhs_span, rhs_span);
                }
                Task::Expr(expr_ast) => self.do_task_expr(expr_ast),
                Task::Index(index_span, lhs_span, rhs_span) => {
                    self.do_task_index(index_span, lhs_span, rhs_span);
                }
                Task::ListLiteral(spans) => self.do_task_list_literal(spans),
                Task::LogBranch(identity, op_span, lhs_span, rhs_ast) => {
                    self.do_task_log_branch(
                        identity, op_span, lhs_span, rhs_ast,
                    );
                }
                Task::LogJoin(identity, size) => {
                    self.do_task_log_join(identity, size);
                }
                Task::LogUnify(identity, op_span, lhs_span, rhs_span) => {
                    self.do_task_log_unify(
                        identity, op_span, lhs_span, rhs_span,
                    );
                }
                Task::PhantomExpr(expr_ast) => {
                    self.do_task_phantom_expr(expr_ast)
                }
                Task::PhantomDone(size) => self.do_task_phantom_done(size),
                Task::TupleLiteral(size) => self.do_task_tuple_literal(size),
                Task::UnOp(unop_ast, sub_span) => {
                    self.do_task_unop(unop_ast, sub_span);
                }
            }
        }
        debug_assert_eq!(self.types.len(), 1);
        self.errs.result()?;
        let (expr_type, expr_static) = self.types.pop().unwrap();
        Ok((self.ops, expr_type, expr_static))
    }

    fn do_task_expr(&mut self, subexpr: ExprAst) {
        match subexpr.node {
            ExprAstNode::Apply(func_ast, arg_ast) => {
                self.tasks.push(Task::Apply(func_ast.span, arg_ast.span));
                self.tasks.push(Task::Expr(*arg_ast));
                self.tasks.push(Task::Expr(*func_ast));
            }
            ExprAstNode::BinOp(
                (binop_span, BinOpAst::LogAnd),
                lhs_ast,
                rhs_ast,
            ) => {
                self.tasks.push(Task::LogBranch(
                    true,
                    binop_span,
                    lhs_ast.span,
                    *rhs_ast,
                ));
                self.tasks.push(Task::Expr(*lhs_ast));
            }
            ExprAstNode::BinOp(
                (binop_span, BinOpAst::LogOr),
                lhs_ast,
                rhs_ast,
            ) => {
                self.tasks.push(Task::LogBranch(
                    false,
                    binop_span,
                    lhs_ast.span,
                    *rhs_ast,
                ));
                self.tasks.push(Task::Expr(*lhs_ast));
            }
            ExprAstNode::BinOp(binop_ast, lhs_ast, rhs_ast) => {
                self.tasks.push(Task::BinOp(
                    binop_ast,
                    lhs_ast.span,
                    rhs_ast.span,
                ));
                self.tasks.push(Task::Expr(*rhs_ast));
                self.tasks.push(Task::Expr(*lhs_ast));
            }
            ExprAstNode::BoolLiteral(boolean) => {
                self.push_primitive_literal(
                    ExprType::Boolean,
                    ExprValue::Boolean(boolean),
                );
            }
            ExprAstNode::Conditional(pred_ast, lhs_ast, rhs_ast) => {
                self.tasks.push(Task::CondBranch(
                    pred_ast.span,
                    *lhs_ast,
                    *rhs_ast,
                ));
                self.tasks.push(Task::Expr(*pred_ast));
            }
            ExprAstNode::HereLabel => self.push_here_label(subexpr.span),
            ExprAstNode::Identifier(id) => {
                self.push_identifier(subexpr.span, id);
            }
            ExprAstNode::Index(index_span, lhs_ast, rhs_ast) => {
                self.tasks.push(Task::Index(
                    index_span,
                    lhs_ast.span,
                    rhs_ast.span,
                ));
                self.tasks.push(Task::Expr(*rhs_ast));
                self.tasks.push(Task::Expr(*lhs_ast));
            }
            ExprAstNode::IntLiteral(integer) => {
                self.push_primitive_literal(
                    ExprType::Integer,
                    ExprValue::Integer(integer),
                );
            }
            ExprAstNode::ListLiteral(item_asts) => {
                self.tasks.push(Task::ListLiteral(
                    item_asts.iter().map(|ast| ast.span).collect(),
                ));
                self.tasks.extend(item_asts.into_iter().rev().map(Task::Expr));
            }
            ExprAstNode::Placeholder(_) => unreachable!(),
            ExprAstNode::StrLiteral(string) => {
                self.push_primitive_literal(
                    ExprType::String,
                    ExprValue::String(string),
                );
            }
            ExprAstNode::TupleLiteral(item_asts) => {
                self.tasks.push(Task::TupleLiteral(item_asts.len()));
                self.tasks.extend(item_asts.into_iter().rev().map(Task::Expr));
            }
            ExprAstNode::UnOp(unop_ast, sub_ast) => {
                self.tasks.push(Task::UnOp(unop_ast, sub_ast.span));
                self.tasks.push(Task::Expr(*sub_ast));
            }
        }
    }

    fn do_task_apply(&mut self, func_span: SrcSpan, arg_span: SrcSpan) {
        debug_assert!(self.types.len() >= 2);
        let (arg_type, arg_static) = self.types.pop().unwrap();
        let (func_type, func_static) = self.types.pop().unwrap();
        let param_and_ret = match func_type {
            ExprType::Undefined => {
                self.types.push(UNDEFINED);
                return;
            }
            ExprType::Function(ref param_and_ret) => param_and_ret,
            other_type => {
                self.errs.push(ExprTypeError::CannotCallType {
                    func_span,
                    func_type: other_type,
                });
                self.types.push(UNDEFINED);
                return;
            }
        };
        let ret_type = param_and_ret.1.clone();
        if !arg_type.is_subtype_of(&param_and_ret.0) {
            let param_type = param_and_ret.1.clone();
            self.errs.push(ExprTypeError::CannotCallFuncWithType {
                func_span,
                func_type,
                arg_span,
                arg_type,
                param_type,
            });
            self.types.push(UNDEFINED);
            return;
        }
        let reason = match (func_static, arg_static) {
            (Ok(func_value), Ok(arg_value)) => {
                let func = func_value.unwrap_func();
                match func.call(arg_value) {
                    Ok(result_value) => {
                        debug_assert!(self.ops.len() >= 2);
                        self.ops.pop();
                        self.ops.pop();
                        self.ops.push(E::Op::literal(result_value.clone()));
                        self.types.push((ret_type, Ok(result_value)));
                        return;
                    }
                    Err(error) => ExprNotStaticReason::StaticEvalError {
                        error: ExprEvalError::FuncEvalError {
                            arg_span,
                            error,
                        },
                    },
                }
            }
            (Err(reason), _) | (_, Err(reason)) => reason,
        };
        self.ops.push(self.env.apply_function_op(func_span, arg_span));
        self.types.push((ret_type, Err(reason)));
    }

    fn do_task_binop(
        &mut self,
        binop_ast: (SrcSpan, BinOpAst),
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) {
        debug_assert!(self.types.len() >= 2);
        let (rhs_type, rhs_static) = self.types.pop().unwrap();
        let (lhs_type, lhs_static) = self.types.pop().unwrap();
        if lhs_type == ExprType::Undefined || rhs_type == ExprType::Undefined {
            self.types.push(UNDEFINED);
            return;
        }
        let op_span = binop_ast.0;
        match self.errs.ok(ExprBinOp::typecheck(
            binop_ast, lhs_span, lhs_type, rhs_span, rhs_type,
        )) {
            Some((binop, result_type)) => match (lhs_static, rhs_static) {
                (Ok(lhs_value), Ok(rhs_value)) => {
                    match binop.evaluate(lhs_value, rhs_value) {
                        Ok(result_value) => {
                            debug_assert!(self.ops.len() >= 2);
                            self.ops.pop();
                            self.ops.pop();
                            self.ops
                                .push(E::Op::literal(result_value.clone()));
                            self.types.push((result_type, Ok(result_value)));
                        }
                        Err(binop_error) => {
                            self.ops.push(self.env.binary_operation_op(
                                binop, op_span, lhs_span, rhs_span,
                            ));
                            let reason =
                                ExprNotStaticReason::StaticEvalError {
                                    error: binop_error.into_expr_eval_error(
                                        op_span, lhs_span, rhs_span,
                                    ),
                                };
                            self.types.push((result_type, Err(reason)));
                        }
                    }
                }
                (Err(reason), _) | (_, Err(reason)) => {
                    self.ops.push(self.env.binary_operation_op(
                        binop, op_span, lhs_span, rhs_span,
                    ));
                    self.types.push((result_type, Err(reason)));
                }
            },
            None => self.types.push(UNDEFINED),
        }
    }

    fn do_task_cond_branch(
        &mut self,
        pred_span: SrcSpan,
        lhs_ast: ExprAst,
        rhs_ast: ExprAst,
    ) {
        debug_assert!(!self.types.is_empty());
        let (pred_type, pred_static) = self.types.pop().unwrap();
        if !pred_type.is_subtype_of(&ExprType::Boolean) {
            self.errs.push(ExprTypeError::CannotUseTypeAsPredicate {
                expr_span: pred_span,
                expr_type: pred_type,
            });
        }
        match pred_static {
            Ok(ExprValue::Boolean(true)) => {
                self.tasks.push(Task::CondUnify(
                    Ok(true),
                    lhs_ast.span,
                    rhs_ast.span,
                ));
                self.tasks.push(Task::PhantomExpr(rhs_ast));
                self.tasks.push(Task::Expr(lhs_ast));
                debug_assert!(!self.ops.is_empty());
                self.ops.pop();
            }
            Ok(ExprValue::Boolean(false)) => {
                self.tasks.push(Task::CondUnify(
                    Ok(false),
                    lhs_ast.span,
                    rhs_ast.span,
                ));
                self.tasks.push(Task::Expr(rhs_ast));
                self.tasks.push(Task::PhantomExpr(lhs_ast));
                debug_assert!(!self.ops.is_empty());
                self.ops.pop();
            }
            other => {
                self.tasks.push(Task::CondElse(
                    if let Err(reason) = other {
                        reason
                    } else {
                        ExprNotStaticReason::TypeError
                    },
                    self.ops.len(),
                    lhs_ast.span,
                    rhs_ast,
                ));
                self.tasks.push(Task::Expr(lhs_ast));
                // Add a placeholder operation, which will be replaced by the
                // `CondElse` task.
                self.ops.push(E::Op::skip_unless(0));
            }
        }
    }

    fn do_task_cond_else(
        &mut self,
        reason: ExprNotStaticReason,
        op_index: usize,
        lhs_span: SrcSpan,
        rhs_ast: ExprAst,
    ) {
        debug_assert!(self.ops.len() > op_index);
        self.ops[op_index] = E::Op::skip_unless(self.ops.len() - op_index);
        self.tasks.push(Task::CondUnify(Err(reason), lhs_span, rhs_ast.span));
        self.tasks.push(Task::CondJoin(self.ops.len()));
        self.tasks.push(Task::Expr(rhs_ast));
        // Add a placeholder operation, which will be replaced by the
        // `CondJoin` task.
        self.ops.push(E::Op::skip(0));
    }

    fn do_task_cond_join(&mut self, op_index: usize) {
        debug_assert!(self.ops.len() > op_index);
        self.ops[op_index] = E::Op::skip(self.ops.len() - (op_index + 1));
    }

    fn do_task_cond_unify(
        &mut self,
        pred_static: Result<bool, ExprNotStaticReason>,
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) {
        debug_assert!(self.types.len() >= 2);
        let (rhs_type, rhs_static) = self.types.pop().unwrap();
        let (lhs_type, lhs_static) = self.types.pop().unwrap();
        let cond_type = lhs_type.union(&rhs_type).unwrap_or_else(|| {
            self.errs.push(ExprTypeError::ConditionBranchesMustBeSameType {
                true_branch_span: lhs_span,
                true_branch_type: lhs_type,
                false_branch_span: rhs_span,
                false_branch_type: rhs_type,
            });
            ExprType::Undefined
        });
        let cond_static = match pred_static {
            Ok(true) => lhs_static,
            Ok(false) => rhs_static,
            Err(reason) => Err(reason),
        };
        self.types.push((cond_type, cond_static));
    }

    fn push_here_label(&mut self, span: SrcSpan) {
        match self.errs.ok(self.env.typecheck_here_label(span)) {
            Some((op, static_label)) => {
                self.ops.push(op);
                self.types.push((ExprType::Label, static_label));
            }
            None => self.types.push(UNDEFINED),
        }
    }

    fn push_identifier(&mut self, span: SrcSpan, name: Rc<str>) {
        match self.errs.ok(self.env.typecheck_identifier(span, &name)) {
            Some((op, id_type, id_static)) => {
                self.ops.push(op);
                self.types.push((id_type, id_static));
            }
            None => self.types.push(UNDEFINED),
        }
    }

    fn do_task_index(
        &mut self,
        bracket_span: SrcSpan,
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) {
        debug_assert!(self.types.len() >= 2);
        let (rhs_type, rhs_static) = self.types.pop().unwrap();
        let (lhs_type, lhs_static) = self.types.pop().unwrap();
        match (lhs_type, rhs_type) {
            (_, ExprType::Undefined) | (ExprType::Undefined, _) => {
                self.types.push(UNDEFINED);
            }
            (ExprType::List(item_type), ExprType::Integer) => {
                let item_type = Rc::unwrap_or_clone(item_type);
                match (lhs_static, rhs_static) {
                    (Ok(lhs_value), Ok(rhs_value)) => {
                        let index = rhs_value.unwrap_int();
                        let item_values = lhs_value.unwrap_list();
                        if index < BigInt::ZERO
                            || index >= BigInt::from(item_values.len())
                        {
                            let reason =
                                ExprNotStaticReason::StaticEvalError {
                                    error:
                                        ExprEvalError::ListIndexOutOfBounds {
                                            list_span: lhs_span,
                                            list_length: item_values.len(),
                                            index_span: rhs_span,
                                            index_value: index,
                                        },
                                };
                            self.types.push((item_type, Err(reason)));
                        } else {
                            debug_assert!(self.ops.len() >= 2);
                            self.ops.pop();
                            self.ops.pop();
                            let item_value = item_values
                                [usize::try_from(index).unwrap()]
                            .clone();
                            self.ops.push(E::Op::literal(item_value.clone()));
                            self.types.push((item_type, Ok(item_value)));
                        }
                    }
                    (Err(reason), _) | (_, Err(reason)) => {
                        let op = self.env.list_index_op(lhs_span, rhs_span);
                        self.ops.push(op);
                        self.types.push((item_type, Err(reason)));
                    }
                }
            }
            (ExprType::List(_), rhs_type) => {
                self.errs.push(ExprTypeError::CannotUseTypeAsIndex {
                    index_span: rhs_span,
                    index_type: rhs_type,
                });
                self.types.push(UNDEFINED);
            }
            (ExprType::Tuple(item_types), ExprType::Integer) => {
                match rhs_static {
                    Ok(rhs_value) => {
                        debug_assert!(!self.ops.is_empty());
                        self.ops.pop();
                        let index = rhs_value.unwrap_int();
                        if index < BigInt::ZERO
                            || index >= BigInt::from(item_types.len())
                        {
                            self.errs.push(
                                ExprTypeError::TupleIndexOutOfRange {
                                    tuple_span: lhs_span,
                                    item_types,
                                    index_span: rhs_span,
                                    index_value: index,
                                },
                            );
                            self.types.push(UNDEFINED);
                            return;
                        }
                        let index = usize::try_from(index).unwrap();
                        let item_type = item_types[index].clone();
                        let item_static = match lhs_static {
                            Ok(lhs_value) => {
                                debug_assert!(!self.ops.is_empty());
                                self.ops.pop();
                                let item_value =
                                    lhs_value.unwrap_tuple()[index].clone();
                                self.ops
                                    .push(E::Op::literal(item_value.clone()));
                                Ok(item_value)
                            }
                            Err(reason) => {
                                self.ops.push(E::Op::tuple_item(index));
                                Err(reason)
                            }
                        };
                        self.types.push((item_type, item_static));
                    }
                    Err(reason) => {
                        self.errs.push(ExprTypeError::TupleIndexNotStatic {
                            index_span: rhs_span,
                            reason,
                        });
                        self.types.push(UNDEFINED);
                    }
                }
            }
            (ExprType::Tuple(_), rhs_type) => {
                self.errs.push(ExprTypeError::CannotUseTypeAsIndex {
                    index_span: rhs_span,
                    index_type: rhs_type,
                });
                self.types.push(UNDEFINED);
            }
            (lhs_type, _) => {
                self.errs.push(ExprTypeError::CannotIndexIntoType {
                    bracket_span,
                    indexed_span: lhs_span,
                    indexed_type: lhs_type,
                });
                self.types.push(UNDEFINED);
            }
        }
    }

    fn do_task_list_literal(&mut self, item_spans: Vec<SrcSpan>) {
        let num_items = item_spans.len();
        let (item_types, static_values) = self.pop_types(num_items);
        let mut item_type = ExprType::Bottom;
        if num_items > 0 {
            let first_item_span = item_spans[0];
            for (ty, span) in item_types.into_iter().zip(item_spans) {
                if let Some(unified) = item_type.union(&ty) {
                    item_type = unified;
                } else {
                    let error = ExprTypeError::ListItemsMustAllBeSameType {
                        first_item_span,
                        first_item_type: item_type.clone(),
                        other_item_span: span,
                        other_item_type: ty,
                    };
                    self.errs.push(error);
                    break;
                }
            }
        };
        let list_static = match static_values {
            Ok(item_values) => {
                debug_assert!(self.ops.len() >= item_values.len());
                self.ops.truncate(self.ops.len() - item_values.len());
                let value = ExprValue::List(Rc::from(item_values));
                self.ops.push(E::Op::literal(value.clone()));
                Ok(value)
            }
            Err(reason) => {
                self.ops.push(E::Op::make_list(num_items));
                Err(reason)
            }
        };
        self.types.push((ExprType::List(Rc::from(item_type)), list_static));
    }

    fn do_task_log_branch(
        &mut self,
        identity: bool,
        op_span: SrcSpan,
        lhs_span: SrcSpan,
        rhs_ast: ExprAst,
    ) {
        self.tasks.push(Task::LogUnify(
            identity,
            op_span,
            lhs_span,
            rhs_ast.span,
        ));
        debug_assert!(!self.types.is_empty());
        match self.types.last().unwrap() {
            (_, Ok(ExprValue::Boolean(value))) if *value == identity => {
                self.tasks.push(Task::Expr(rhs_ast));
                debug_assert!(!self.ops.is_empty());
                self.ops.pop();
            }
            (_, Ok(ExprValue::Boolean(_))) => {
                self.tasks.push(Task::PhantomExpr(rhs_ast));
            }
            (_, _) => {
                self.tasks.push(Task::LogJoin(identity, self.ops.len()));
                self.tasks.push(Task::Expr(rhs_ast));
                // Add a placeholder operation, which will be replaced with
                // `skip_if` or `skip_unless` by the `LogUnify` task.
                self.ops.push(E::Op::skip(0));
            }
        }
    }

    fn do_task_log_join(&mut self, identity: bool, op_index: usize) {
        debug_assert!(self.ops.len() > op_index);
        self.ops[op_index] = if identity {
            E::Op::skip_unless(self.ops.len() - op_index)
        } else {
            E::Op::skip_if(self.ops.len() - op_index)
        };
        self.ops.push(E::Op::skip(1));
        self.ops.push(E::Op::literal(ExprValue::Boolean(!identity)));
    }

    fn do_task_log_unify(
        &mut self,
        identity: bool,
        op_span: SrcSpan,
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) {
        debug_assert!(self.types.len() >= 2);
        let (rhs_type, rhs_static) = self.types.pop().unwrap();
        let (lhs_type, lhs_static) = self.types.pop().unwrap();
        self.types.push(match (lhs_type, rhs_type) {
            (ExprType::Boolean, ExprType::Boolean) => {
                let static_value = match &lhs_static {
                    Ok(ExprValue::Boolean(value)) => {
                        if *value == identity {
                            rhs_static
                        } else {
                            lhs_static
                        }
                    }
                    _ => lhs_static,
                };
                (ExprType::Boolean, static_value)
            }
            (ExprType::Undefined, _) | (_, ExprType::Undefined) => UNDEFINED,
            (lhs_type, rhs_type) => {
                self.errs.push(ExprTypeError::CannotApplyBinaryOpToTypes {
                    op_span,
                    op: if identity {
                        BinOpAst::LogAnd
                    } else {
                        BinOpAst::LogOr
                    },
                    lhs_span,
                    lhs_type,
                    rhs_span,
                    rhs_type,
                });
                UNDEFINED
            }
        });
    }

    fn do_task_phantom_expr(&mut self, expr_ast: ExprAst) {
        self.tasks.push(Task::PhantomDone(self.ops.len()));
        self.tasks.push(Task::Expr(expr_ast));
    }

    fn do_task_phantom_done(&mut self, size: usize) {
        // Delete all operations that would have been used to calculate the
        // expression value.
        debug_assert!(self.ops.len() >= size);
        self.ops.truncate(size);
        // Leave the expression's type on the type stack, but remove any static
        // value associated with it.
        debug_assert!(!self.types.is_empty());
        self.types.last_mut().unwrap().1 = Err(ExprNotStaticReason::Phantom);
    }

    fn push_primitive_literal(&mut self, ty: ExprType, value: ExprValue) {
        self.ops.push(E::Op::literal(value.clone()));
        self.types.push((ty, Ok(value)));
    }

    fn do_task_tuple_literal(&mut self, num_items: usize) {
        let (item_types, static_values) = self.pop_types(num_items);
        let tuple_static = match static_values {
            Ok(item_values) => {
                debug_assert!(self.ops.len() >= item_values.len());
                self.ops.truncate(self.ops.len() - item_values.len());
                let tuple_value = ExprValue::Tuple(Rc::from(item_values));
                self.ops.push(E::Op::literal(tuple_value.clone()));
                Ok(tuple_value)
            }
            Err(reason) => {
                self.ops.push(E::Op::make_tuple(num_items));
                Err(reason)
            }
        };
        self.types.push((ExprType::Tuple(Rc::from(item_types)), tuple_static));
    }

    fn do_task_unop(
        &mut self,
        unop_ast: (SrcSpan, UnOpAst),
        arg_span: SrcSpan,
    ) {
        debug_assert!(!self.types.is_empty());
        let (arg_type, arg_static) = self.types.pop().unwrap();
        if arg_type == ExprType::Undefined {
            self.types.push(UNDEFINED);
            return;
        }
        let op_span = unop_ast.0;
        match self.errs.ok(ExprUnOp::typecheck(unop_ast, arg_span, arg_type)) {
            Some((unop, result_type)) => match arg_static {
                Ok(arg_value) => match unop.evaluate(arg_value) {
                    Ok(result_value) => {
                        debug_assert!(!self.ops.is_empty());
                        self.ops.pop();
                        self.ops.push(E::Op::literal(result_value.clone()));
                        self.types.push((result_type, Ok(result_value)));
                    }
                    Err(unop_error) => {
                        self.ops.push(
                            self.env
                                .unary_operation_op(unop, op_span, arg_span),
                        );
                        let reason = ExprNotStaticReason::StaticEvalError {
                            error: unop_error
                                .into_expr_eval_error(op_span, arg_span),
                        };
                        self.types.push((result_type, Err(reason)));
                    }
                },
                Err(reason) => {
                    self.ops.push(
                        self.env.unary_operation_op(unop, op_span, arg_span),
                    );
                    self.types.push((result_type, Err(reason)));
                }
            },
            None => self.types.push(UNDEFINED),
        }
    }

    fn pop_types(
        &mut self,
        num_items: usize,
    ) -> (Vec<ExprType>, Result<Vec<ExprValue>, ExprNotStaticReason>) {
        debug_assert!(num_items <= self.types.len());
        let (item_types, static_values): (Vec<_>, Vec<ExprStatic>) =
            self.types.drain((self.types.len() - num_items)..).unzip();
        (item_types, static_values.into_iter().collect())
    }
}

//===========================================================================//
