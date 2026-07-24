use super::binop::ExprBinOp;
use super::error::ExprTypeResult;
use super::unop::ExprUnOp;
use super::value::{ExprType, ExprValue};
use crate::error::SrcSpan;
use std::rc::Rc;

//===========================================================================//

/// A type environment in which expressions can be typechecked.
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

    /// Returns an operation to apply a function to a value.
    fn apply_function_op(&self, arg_span: SrcSpan) -> Self::Op;

    /// Returns an operation to combine the top two stack values with the
    /// specified binary operation.
    fn binary_operation_op(
        &self,
        binop: ExprBinOp,
        op_span: SrcSpan,
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) -> Self::Op;

    /// Returns an operation to index into a list.
    fn list_index_op(
        &self,
        list_span: SrcSpan,
        index_span: SrcSpan,
    ) -> Self::Op;
}

//===========================================================================//

/// A type that represents a single operation for an expression stack machine.
pub(crate) trait ExprOp {
    /// Returns an operation to push a single literal value onto the stack.
    fn literal(value: ExprValue) -> Self;

    /// Returns an operation to collect the top `num_items` stack values (which
    /// must all have the same type) into a list value.
    fn make_list(num_items: usize) -> Self;

    /// Returns an operation to collect the top `num_items` stack values into a
    /// tuple value.
    fn make_tuple(num_items: usize) -> Self;

    /// Returns an operation to unconditionally skip over the specified number
    /// of operations.
    fn skip(offset: usize) -> Self;

    /// Returns an operation to pop a boolean from the stack, and skip over the
    /// specified number of operations if that boolean is true.
    fn skip_if(offset: usize) -> Self;

    /// Returns an operation to pop a boolean from the stack, and skip over the
    /// specified number of operations if that boolean is false.
    fn skip_unless(offset: usize) -> Self;

    /// Returns an operation to index into a tuple.
    fn tuple_item(index: usize) -> Self;

    /// Returns an operation to modify the top stack value with the specified
    /// unary operation.
    fn unary_operation(unop: ExprUnOp) -> Self;
}

//===========================================================================//
