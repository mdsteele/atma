use super::binop::{ExprBinOp, ExprBinOpEvalError};
use super::unop::ExprUnOp;
use super::value::{ExprType, ExprValue};
use crate::parse::{
    BinOpAst, ExprAst, ExprAstNode, ParseError, ParseResult, SrcSpan, UnOpAst,
};
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

pub(crate) trait ExprEnv {
    type Op: ExprOp;

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        id: &str,
    ) -> ParseResult<(Self::Op, ExprType, Option<ExprValue>)>;
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
    errors: Vec<ParseError>,
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
    ) -> ParseResult<(Vec<E::Op>, ExprType, Option<ExprValue>)> {
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
                ExprAstNode::Identifier(_) => {}
                ExprAstNode::Index(_, lhs, rhs) => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                ExprAstNode::IntLiteral(_) => {}
                ExprAstNode::ListLiteral(items) => stack.extend(items),
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
                    debug_assert!(self.ops.len() >= 2);
                    self.ops.pop();
                    self.ops.pop();
                    match binop.evaluate(lhs_value, rhs_value) {
                        Ok(result_value) => {
                            self.ops
                                .push(E::Op::literal(result_value.clone()));
                            self.types.push((result_type, Some(result_value)));
                        }
                        Err(error) => {
                            self.binop_eval_error(error, lhs_span, rhs_span);
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

    fn typecheck_identifier(&mut self, span: SrcSpan, id: &str) {
        match self.env.typecheck_identifier(span, id) {
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
        index_span: SrcSpan,
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
                        let message = "list index is statically out of range"
                            .to_string();
                        let label1 = format!(
                            "this list has length {}",
                            item_values.len()
                        );
                        let label2 =
                            format!("the value of this expression is {index}");
                        self.errors.push(
                            ParseError::new(rhs_ast.span, message)
                                .with_label(lhs_ast.span, label1)
                                .with_label(rhs_ast.span, label2),
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
                let message = format!("cannot use {rhs_type} as a list index");
                let label = format!("this expression has type {rhs_type}");
                self.errors.push(
                    ParseError::new(rhs_ast.span, message)
                        .with_label(rhs_ast.span, label),
                );
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
                        let message = "tuple index out of bounds".to_string();
                        let label1 = format!(
                            "this expression has type {}",
                            ExprType::Tuple(item_types)
                        );
                        let label2 =
                            format!("the value of this expression is {index}");
                        self.errors.push(
                            ParseError::new(rhs_ast.span, message)
                                .with_label(lhs_ast.span, label1)
                                .with_label(rhs_ast.span, label2),
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
                    let message = "tuple index must be static".to_string();
                    let label = "this expression isn't static".to_string();
                    self.errors.push(
                        ParseError::new(rhs_ast.span, message)
                            .with_label(rhs_ast.span, label),
                    );
                    self.types.push((ExprType::Bottom, None));
                }
            }
            (ExprType::Tuple(_), rhs_type) => {
                let message =
                    format!("cannot use {rhs_type} as a tuple index");
                let label = format!("this expression has type {rhs_type}");
                self.errors.push(
                    ParseError::new(rhs_ast.span, message)
                        .with_label(rhs_ast.span, label),
                );
                self.types.push((ExprType::Bottom, None));
            }
            (lhs_type, _) => {
                let message =
                    format!("cannot index into value of type {lhs_type}");
                let label = format!("this expression has type {lhs_type}");
                self.errors.push(
                    ParseError::new(index_span, message)
                        .with_label(lhs_ast.span, label),
                );
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
                let message =
                    "all items in a list must have the same type".to_string();
                let label1 = format!("this item has type {item_type}");
                let label2 = format!("this item has type {ty}");
                self.errors.push(
                    ParseError::new(ast.span, message)
                        .with_label(item_asts[0].span, label1)
                        .with_label(ast.span, label2),
                );
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
        _lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) {
        match error {
            ExprBinOpEvalError::DivideByZero => {
                let message = "cannot divide by zero".to_string();
                let label = "the value of this expression is 0".to_string();
                self.errors.push(
                    ParseError::new(rhs_span, message)
                        .with_label(rhs_span, label),
                );
            }
            ExprBinOpEvalError::ModByZero => {
                let message = "cannot modulo by zero".to_string();
                let label = "the value of this expression is 0".to_string();
                self.errors.push(
                    ParseError::new(rhs_span, message)
                        .with_label(rhs_span, label),
                );
            }
            ExprBinOpEvalError::PowNegativeExponent(exponent) => {
                let message = "exponent must be non-negative".to_string();
                let label =
                    format!("the value of this expression is {exponent}");
                self.errors.push(
                    ParseError::new(rhs_span, message)
                        .with_label(rhs_span, label),
                );
            }
            ExprBinOpEvalError::BitShiftByNegative(shift) => {
                let message =
                    "cannot shift by a negative number of bits".to_string();
                let label = format!("the value of this expression is {shift}");
                self.errors.push(
                    ParseError::new(rhs_span, message)
                        .with_label(rhs_span, label),
                );
            }
            ExprBinOpEvalError::BitShiftOutOfRange(shift) => {
                let message = "shift by too many bits".to_string();
                let label = format!("the value of this expression is {shift}");
                self.errors.push(
                    ParseError::new(rhs_span, message)
                        .with_label(rhs_span, label),
                );
            }
        }
    }
}

//===========================================================================//
