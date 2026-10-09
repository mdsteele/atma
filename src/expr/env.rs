use super::binop::ExprBinOp;
use super::error::{ExprStatic, ExprTypeResult};
use super::label::ExprLabel;
use super::template::Template;
use super::unop::ExprUnOp;
use super::value::{ExprType, ExprValue};
use crate::error::SrcSpan;
use crate::parse::HereLabelKind;
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

/// A type environment in which expressions can be typechecked.
pub(crate) trait ExprEnv {
    type Op: ExprOp;

    fn typecheck_here_label(
        &self,
        span: SrcSpan,
        kind: HereLabelKind,
    ) -> ExprTypeResult<(Self::Op, ExprStatic)>;

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, ExprStatic)>;

    /// Returns an operation to apply a function to a value.
    fn apply_function_op(
        &self,
        func_span: SrcSpan,
        arg_span: SrcSpan,
    ) -> Self::Op;

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

    /// Returns an operation to modify the top stack value with the specified
    /// unary operation.
    fn unary_operation_op(
        &self,
        unop: ExprUnOp,
        op_span: SrcSpan,
        arg_span: SrcSpan,
    ) -> Self::Op;

    /// Returns an operation to use the top stack value as an integer address
    /// to create a label in the current address space.
    fn with_addr_op(&self, op_span: SrcSpan) -> ExprTypeResult<Self::Op>;

    /// Returns a label in the current address space with the given static
    /// integer address.
    fn with_addr_static(
        &self,
        op_span: SrcSpan,
        address: BigInt,
    ) -> ExprTypeResult<ExprLabel>;
}

//===========================================================================//

/// A type that represents a single operation for an expression stack machine.
pub(crate) trait ExprOp {
    /// Returns an operation to pop a value from the stack, interpolate it into
    /// a template, and push the resulting string onto the stack.
    fn interpolate(template: Template) -> Self;

    /// Returns an operation to push a single literal value onto the stack.
    fn literal(value: ExprValue) -> Self;

    /// Returns an operation to collect the top `num_items` stack values (which
    /// must all have the same type) into a list value.
    fn make_list(num_items: usize) -> Self;

    /// Returns an operation to collect the top `num_items` stack values into a
    /// tuple value.
    fn make_tuple(num_items: usize) -> Self;

    /// Returns an operation to use the top stack value as an integer address
    /// to read a byte from the simulated memory bus.
    fn memory_read() -> Self;

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
}

//===========================================================================//
