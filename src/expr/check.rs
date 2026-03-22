use super::binop::{ExprBinOp, ExprBinOpEvalError};
use super::error::{ExprEvalError, ExprTypeError, ExprTypeResult};
use super::unop::ExprUnOp;
use super::value::{ExprType, ExprValue};
use crate::error::SrcSpan;
use crate::parse::{BinOpAst, ExprAst, ExprAstNode, UnOpAst};
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

pub(crate) trait ExprEnv {
    type Op: ExprOp;

    fn typecheck_here_label(
        &self,
        span: SrcSpan,
    ) -> ExprTypeResult<(Self::Op, Option<ExprValue>)>;

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, Option<ExprValue>)>;
}

/// A type that represents a single operation for an expression stack machine.
pub(crate) trait ExprOp {
    /// Returns an operation to combine the top two stack values with the
    /// specified binary operation.
    fn binary_operation(binop: ExprBinOp) -> Self;

    /// Returns an operation to index into a list.
    fn list_index() -> Self;

    /// Returns an operation to push a single literal value onto the stack.
    fn literal(value: ExprValue) -> Self;

    /// Returns an operation to collect the top `num_items` stack values (which
    /// must all have the same type) into a list value.
    fn make_list(num_items: usize) -> Self;

    /// Returns an operation to collect the top `num_items` stack values into a
    /// tuple value.
    fn make_tuple(num_items: usize) -> Self;

    /// Returns an operation to index into a tuple.
    fn tuple_item(index: usize) -> Self;

    /// Returns an operation to modify the top stack value with the specified
    /// unary operation.
    fn unary_operation(unop: ExprUnOp) -> Self;
}

//===========================================================================//

pub(crate) struct ExprCompiler<'a, E: ExprEnv> {
    env: &'a E,
    types: Vec<(ExprType, Option<ExprValue>)>,
    ops: Vec<E::Op>,
    errors: Vec<ExprTypeError>,
}

impl<'a, E: ExprEnv> ExprCompiler<'a, E> {
    pub(crate) fn new(env: &'a E) -> ExprCompiler<'a, E> {
        ExprCompiler {
            env,
            types: Vec::new(),
            ops: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub(crate) fn typecheck(
        mut self,
        expr: &ExprAst,
    ) -> ExprTypeResult<(Vec<E::Op>, ExprType, Option<ExprValue>)> {
        let mut stack = vec![expr];
        let mut subexprs = Vec::<&ExprAst>::new();
        while let Some(subexpr) = stack.pop() {
            subexprs.push(subexpr);
            match &subexpr.node {
                ExprAstNode::BinOp(_, lhs, rhs) => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                ExprAstNode::BoolLiteral(_) => {}
                ExprAstNode::HereLabel => {}
                ExprAstNode::Identifier(_) => {}
                ExprAstNode::Index(_, lhs, rhs) => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                ExprAstNode::IntLiteral(_) => {}
                ExprAstNode::ListLiteral(items) => stack.extend(items),
                ExprAstNode::Placeholder(_) => {}
                ExprAstNode::StrLiteral(_) => {}
                ExprAstNode::TupleLiteral(items) => stack.extend(items),
                ExprAstNode::UnOp(_, sub) => stack.push(sub),
            }
        }
        for subexpr in subexprs.into_iter().rev() {
            self.typecheck_subexpr(subexpr);
        }
        debug_assert_eq!(self.types.len(), 1);
        if self.errors.is_empty() {
            let (expr_type, static_value) = self.types.pop().unwrap();
            Ok((self.ops, expr_type, static_value))
        } else {
            Err(self.errors)
        }
    }

    fn typecheck_subexpr(&mut self, subexpr: &ExprAst) {
        match &subexpr.node {
            ExprAstNode::BinOp(binop_ast, lhs_ast, rhs_ast) => {
                self.typecheck_binop_node(*binop_ast, lhs_ast, rhs_ast);
            }
            ExprAstNode::BoolLiteral(boolean) => {
                self.typecheck_primitive_literal(
                    ExprType::Boolean,
                    ExprValue::Boolean(*boolean),
                );
            }
            ExprAstNode::HereLabel => {
                self.typecheck_here_label(subexpr.span);
            }
            ExprAstNode::Identifier(id) => {
                self.typecheck_identifier(subexpr.span, id);
            }
            ExprAstNode::Index(index_span, lhs_ast, rhs_ast) => {
                self.typecheck_index(*index_span, lhs_ast, rhs_ast);
            }
            ExprAstNode::IntLiteral(integer) => {
                self.typecheck_primitive_literal(
                    ExprType::Integer,
                    ExprValue::Integer(integer.clone()),
                );
            }
            ExprAstNode::ListLiteral(item_asts) => {
                self.typecheck_list_literal(item_asts);
            }
            ExprAstNode::Placeholder(_) => unreachable!(),
            ExprAstNode::StrLiteral(string) => {
                self.typecheck_primitive_literal(
                    ExprType::String,
                    ExprValue::String(string.clone()),
                );
            }
            ExprAstNode::TupleLiteral(item_asts) => {
                self.typecheck_tuple_literal(item_asts);
            }
            ExprAstNode::UnOp(unop_ast, sub_ast) => {
                self.typecheck_unop_node(*unop_ast, sub_ast);
            }
        }
    }

    fn typecheck_binop_node(
        &mut self,
        binop_ast: (SrcSpan, BinOpAst),
        lhs_ast: &ExprAst,
        rhs_ast: &ExprAst,
    ) {
        debug_assert!(self.types.len() >= 2);
        let (rhs_type, rhs_static) = self.types.pop().unwrap();
        let (lhs_type, lhs_static) = self.types.pop().unwrap();
        if lhs_type == ExprType::Bottom || rhs_type == ExprType::Bottom {
            self.types.push((ExprType::Bottom, None));
            return;
        }
        let lhs_span = lhs_ast.span;
        let rhs_span = rhs_ast.span;
        match ExprBinOp::typecheck(
            binop_ast, lhs_span, lhs_type, rhs_span, rhs_type,
        ) {
            Ok((binop, result_type)) => {
                if let Some(lhs_value) = lhs_static
                    && let Some(rhs_value) = rhs_static
                {
                    match binop.evaluate(lhs_value, rhs_value) {
                        Ok(result_value) => {
                            debug_assert!(self.ops.len() >= 2);
                            self.ops.pop();
                            self.ops.pop();
                            self.ops
                                .push(E::Op::literal(result_value.clone()));
                            self.types.push((result_type, Some(result_value)));
                        }
                        Err(ExprBinOpEvalError::UnresolvedLabel) => {
                            self.ops.push(E::Op::binary_operation(binop));
                            self.types.push((result_type, None));
                        }
                        Err(error) => {
                            self.binop_eval_error(
                                error,
                                binop_ast.0,
                                lhs_span,
                                rhs_span,
                            );
                            self.types.push((result_type, None));
                        }
                    }
                } else {
                    self.ops.push(E::Op::binary_operation(binop));
                    self.types.push((result_type, None));
                }
            }
            Err(mut errors) => {
                self.errors.append(&mut errors);
                self.types.push((ExprType::Bottom, None));
            }
        }
    }

    fn typecheck_here_label(&mut self, span: SrcSpan) {
        match self.env.typecheck_here_label(span) {
            Ok((op, static_label)) => {
                self.ops.push(op);
                self.types.push((ExprType::Label, static_label));
            }
            Err(mut errors) => {
                self.errors.append(&mut errors);
                self.types.push((ExprType::Bottom, None));
            }
        }
    }

    fn typecheck_identifier(&mut self, span: SrcSpan, name: &Rc<str>) {
        match self.env.typecheck_identifier(span, name) {
            Ok((op, id_type, id_static)) => {
                self.ops.push(op);
                self.types.push((id_type, id_static));
            }
            Err(mut errors) => {
                self.errors.append(&mut errors);
                self.types.push((ExprType::Bottom, None));
            }
        }
    }

    fn typecheck_index(
        &mut self,
        bracket_span: SrcSpan,
        lhs_ast: &ExprAst,
        rhs_ast: &ExprAst,
    ) {
        debug_assert!(self.types.len() >= 2);
        let (rhs_type, rhs_static) = self.types.pop().unwrap();
        let (lhs_type, lhs_static) = self.types.pop().unwrap();
        match (lhs_type, rhs_type) {
            (_, ExprType::Bottom) | (ExprType::Bottom, _) => {
                self.types.push((ExprType::Bottom, None));
            }
            (ExprType::List(item_type), ExprType::Integer) => {
                let item_type = Rc::unwrap_or_clone(item_type);
                if let Some(lhs_value) = lhs_static
                    && let Some(rhs_value) = rhs_static
                {
                    debug_assert!(self.ops.len() >= 2);
                    self.ops.pop();
                    self.ops.pop();
                    let index = rhs_value.unwrap_int();
                    let item_values = lhs_value.unwrap_list();
                    if index < BigInt::ZERO
                        || index >= BigInt::from(item_values.len())
                    {
                        self.errors.push(
                            ExprTypeError::ListIndexStaticallyOutOfRange {
                                list_span: lhs_ast.span,
                                list_length: item_values.len(),
                                index_span: rhs_ast.span,
                                index_value: index,
                            },
                        );
                        self.types.push((ExprType::Bottom, None));
                    } else {
                        let result_value = item_values
                            [usize::try_from(index).unwrap()]
                        .clone();
                        self.ops.push(E::Op::literal(result_value.clone()));
                        self.types.push((item_type, Some(result_value)));
                    }
                } else {
                    self.ops.push(E::Op::list_index());
                    self.types.push((item_type, None));
                }
            }
            (ExprType::List(_), rhs_type) => {
                self.errors.push(ExprTypeError::CannotUseTypeAsIndex {
                    index_span: rhs_ast.span,
                    index_type: rhs_type,
                });
                self.types.push((ExprType::Bottom, None));
            }
            (ExprType::Tuple(item_types), ExprType::Integer) => {
                if let Some(rhs_value) = rhs_static {
                    debug_assert!(!self.ops.is_empty());
                    self.ops.pop();
                    let index = rhs_value.unwrap_int();
                    if index < BigInt::ZERO
                        || index >= BigInt::from(item_types.len())
                    {
                        self.errors.push(
                            ExprTypeError::TupleIndexOutOfRange {
                                tuple_span: lhs_ast.span,
                                item_types,
                                index_span: rhs_ast.span,
                                index_value: index,
                            },
                        );
                        self.types.push((ExprType::Bottom, None));
                        return;
                    }
                    let index = usize::try_from(index).unwrap();
                    let item_type = item_types[index].clone();
                    if let Some(lhs_value) = lhs_static {
                        debug_assert!(!self.ops.is_empty());
                        self.ops.pop();
                        let item_value =
                            lhs_value.unwrap_tuple()[index].clone();
                        self.ops.push(E::Op::literal(item_value.clone()));
                        self.types.push((item_type, Some(item_value)));
                    } else {
                        self.ops.push(E::Op::tuple_item(index));
                        self.types.push((item_type, None));
                    }
                } else {
                    self.errors.push(ExprTypeError::TupleIndexNotStatic {
                        index_span: rhs_ast.span,
                    });
                    self.types.push((ExprType::Bottom, None));
                }
            }
            (ExprType::Tuple(_), rhs_type) => {
                self.errors.push(ExprTypeError::CannotUseTypeAsIndex {
                    index_span: rhs_ast.span,
                    index_type: rhs_type,
                });
                self.types.push((ExprType::Bottom, None));
            }
            (lhs_type, _) => {
                self.errors.push(ExprTypeError::CannotIndexIntoType {
                    bracket_span,
                    indexed_span: lhs_ast.span,
                    indexed_type: lhs_type,
                });
                self.types.push((ExprType::Bottom, None));
            }
        }
    }

    fn typecheck_list_literal(&mut self, item_asts: &[ExprAst]) {
        let num_items = item_asts.len();
        let (item_types, static_values) = self.pop_types(num_items);
        let item_type =
            item_types.first().cloned().unwrap_or(ExprType::Bottom);
        for (ty, ast) in item_types.into_iter().zip(item_asts) {
            if ty != item_type {
                self.errors.push(ExprTypeError::ListItemsMustAllBeSameType {
                    first_item_span: item_asts[0].span,
                    first_item_type: item_type.clone(),
                    other_item_span: ast.span,
                    other_item_type: ty,
                });
                break;
            }
        }
        let static_value = if let Some(item_values) = static_values {
            debug_assert!(self.ops.len() >= item_values.len());
            self.ops.truncate(self.ops.len() - item_values.len());
            let value = ExprValue::List(Rc::from(item_values));
            self.ops.push(E::Op::literal(value.clone()));
            Some(value)
        } else {
            self.ops.push(E::Op::make_list(num_items));
            None
        };
        self.types.push((ExprType::List(Rc::from(item_type)), static_value));
    }

    fn typecheck_primitive_literal(&mut self, ty: ExprType, value: ExprValue) {
        self.ops.push(E::Op::literal(value.clone()));
        self.types.push((ty, Some(value)));
    }

    fn typecheck_tuple_literal(&mut self, item_asts: &[ExprAst]) {
        let num_items = item_asts.len();
        let (item_types, static_values) = self.pop_types(num_items);
        let static_value = if let Some(item_values) = static_values {
            debug_assert!(self.ops.len() >= item_values.len());
            self.ops.truncate(self.ops.len() - item_values.len());
            let value = ExprValue::Tuple(Rc::from(item_values));
            self.ops.push(E::Op::literal(value.clone()));
            Some(value)
        } else {
            self.ops.push(E::Op::make_tuple(num_items));
            None
        };
        self.types.push((ExprType::Tuple(Rc::from(item_types)), static_value));
    }

    fn typecheck_unop_node(
        &mut self,
        unop_ast: (SrcSpan, UnOpAst),
        sub_ast: &ExprAst,
    ) {
        debug_assert!(!self.types.is_empty());
        let (sub_type, sub_static) = self.types.pop().unwrap();
        if sub_type == ExprType::Bottom {
            self.types.push((ExprType::Bottom, None));
            return;
        }
        match ExprUnOp::typecheck(unop_ast, sub_ast.span, sub_type) {
            Ok((unop, result_type)) => {
                if let Some(sub_value) = sub_static {
                    debug_assert!(!self.ops.is_empty());
                    self.ops.pop();
                    let result_value = unop.evaluate(sub_value);
                    self.ops.push(E::Op::literal(result_value.clone()));
                    self.types.push((result_type, Some(result_value)));
                } else {
                    self.ops.push(E::Op::unary_operation(unop));
                    self.types.push((result_type, None));
                }
            }
            Err(mut errors) => {
                self.errors.append(&mut errors);
                self.types.push((ExprType::Bottom, None));
            }
        }
    }

    fn pop_types(
        &mut self,
        num_items: usize,
    ) -> (Vec<ExprType>, Option<Vec<ExprValue>>) {
        debug_assert!(num_items <= self.types.len());
        let (item_types, static_values): (Vec<_>, Vec<Option<_>>) =
            self.types.drain((self.types.len() - num_items)..).unzip();
        (item_types, static_values.into_iter().collect())
    }

    fn binop_eval_error(
        &mut self,
        error: ExprBinOpEvalError,
        op_span: SrcSpan,
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) {
        let expr_eval_error = match error {
            ExprBinOpEvalError::BitShiftByNegative(rhs_value) => {
                ExprEvalError::BitShiftByNegative { rhs_span, rhs_value }
            }
            ExprBinOpEvalError::BitShiftOutOfRange(rhs_value) => {
                ExprEvalError::BitShiftOutOfRange { rhs_span, rhs_value }
            }
            ExprBinOpEvalError::DivideByZero => {
                ExprEvalError::DivideByZero { rhs_span }
            }
            ExprBinOpEvalError::ModByZero => {
                ExprEvalError::ModByZero { rhs_span }
            }
            ExprBinOpEvalError::PowNegativeExponent(rhs_value) => {
                ExprEvalError::PowNegativeExponent { rhs_span, rhs_value }
            }
            ExprBinOpEvalError::SubtractLabelsInDifferentAddrspaces(
                lhs_space,
                rhs_space,
            ) => ExprEvalError::SubtractLabelsInDifferentAddrspaces {
                op_span,
                lhs_span,
                lhs_space,
                rhs_span,
                rhs_space,
            },
            ExprBinOpEvalError::UnresolvedLabel => unreachable!(),
        };
        self.errors
            .push(ExprTypeError::StaticEvalError { error: expr_eval_error });
    }
}

//===========================================================================//
