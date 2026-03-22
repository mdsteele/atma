use super::value::ExprType;
use crate::error::{SourceError, SrcSpan, ToSourceError};
use crate::parse::{BinOpAst, UnOpAst};
use num_bigint::{BigInt, BigUint};
use std::rc::Rc;

//===========================================================================//

/// A specialized `Result` type for expression typechecking.
pub type ExprTypeResult<T> = Result<T, Vec<ExprTypeError>>;

//===========================================================================//

/// An error encountered while typechecking an expression.
pub enum ExprTypeError {
    /// Found a binary operator that cannot be applied to its arguments'
    /// expression types.
    CannotApplyBinaryOpToTypes {
        /// The source code span for the binary operator.
        op_span: SrcSpan,
        /// The binary operator.
        op: BinOpAst,
        /// The source code span for the left-hand side of the binary operator.
        lhs_span: SrcSpan,
        /// The expression type of the left-hand side of the binary operator.
        lhs_type: ExprType,
        /// The source code span for the right-hand side of the binary
        /// operator.
        rhs_span: SrcSpan,
        /// The expression type of the right-hand side of the binary operator.
        rhs_type: ExprType,
    },
    /// Found a unary operator that cannot be applied to its argument's
    /// expression type.
    CannotApplyUnaryOpToType {
        /// The source code span for the unary operator.
        op_span: SrcSpan,
        /// The unary operator.
        op: UnOpAst,
        /// The source code span for the argument to the unary operator.
        arg_span: SrcSpan,
        /// The expression type of the argument to the unary operator.
        arg_type: ExprType,
    },
    /// Found an expression of invalid type being indexed into, as though it
    /// were a list or tuple.
    CannotIndexIntoType {
        /// The source code span for the indexing brackets.
        bracket_span: SrcSpan,
        /// The source code span for the expression being indexed.
        indexed_span: SrcSpan,
        /// The expression type of the expression being indexed.
        indexed_type: ExprType,
    },
    /// Found a list or tuple indexing operation with an index expression of
    /// invalid type.
    CannotUseTypeAsIndex {
        /// The source code span for the index expression.
        index_span: SrcSpan,
        /// The expression type of the index expression.
        index_type: ExprType,
    },
    /// Found a list indexing expression with an index value that is statically
    /// known to be out of range.
    ListIndexStaticallyOutOfRange {
        /// The source code span for the list expression.
        list_span: SrcSpan,
        /// The statically-known length of the list.
        list_length: usize,
        /// The source code span for the index expression.
        index_span: SrcSpan,
        /// The statically-known value of the index.
        index_value: BigInt,
    },
    /// Found a list literal expression whose items don't all have the same
    /// expression type.
    ListItemsMustAllBeSameType {
        /// The source code span for the first item in the list.
        first_item_span: SrcSpan,
        /// The expression type of the first item in the list.
        first_item_type: ExprType,
        /// The source code span for another item in the list.
        other_item_span: SrcSpan,
        /// The expression type of the other item in the list.
        other_item_type: ExprType,
    },
    /// Found a relative label (e.g. `$<`) in a debugger script.
    RelativeLabelInDebuggerScript {
        /// The source code span for the relative label.
        span: SrcSpan,
    },
    /// Found a relative label (e.g. `$<`) in a linker config.
    RelativeLabelInLinkerConfig {
        /// The source code span for the relative label.
        span: SrcSpan,
    },
    /// Found a relative label (e.g. `$<`) outside of any `.SECTION` directive.
    RelativeLabelOutsideOfAnySection {
        /// The source code span for the relative label.
        span: SrcSpan,
    },
    /// Encountered an error while evaluating a static expression.
    StaticEvalError {
        /// The evaluation error.
        error: ExprEvalError,
    },
    /// Found a tuple indexing operation with a non-static index expression.
    TupleIndexNotStatic {
        /// The source code span for the index expression.
        index_span: SrcSpan,
    },
    /// Found a tuple indexing operation with an index value that is out of
    /// range.
    TupleIndexOutOfRange {
        /// The source code span for the tuple expression.
        tuple_span: SrcSpan,
        /// The types of the tuple's items.
        item_types: Rc<[ExprType]>,
        /// The source code span for the index expression.
        index_span: SrcSpan,
        /// The (statically-known) value of the index.
        index_value: BigInt,
    },
    /// Found an identifier that was never declared.
    UnknownIdentifier {
        /// The source code span for the identifier.
        span: SrcSpan,
        /// The name of the identifier.
        name: Rc<str>,
    },
}

impl ToSourceError for ExprTypeError {
    fn to_source_error(self) -> SourceError {
        match self {
            ExprTypeError::CannotApplyBinaryOpToTypes {
                op_span,
                op,
                lhs_span,
                lhs_type,
                rhs_span,
                rhs_type,
            } => {
                let (verb, conj, rev) = op.verb_conj_rev();
                let message = if rev {
                    format!("Cannot {verb} {rhs_type} {conj} {lhs_type}")
                } else {
                    format!("Cannot {verb} {lhs_type} {conj} {rhs_type}")
                };
                let lhs_label = format!("this expression has type {lhs_type}");
                let rhs_label = format!("this expression has type {rhs_type}");
                SourceError::new(op_span, message)
                    .with_label(lhs_span, lhs_label)
                    .with_label(rhs_span, rhs_label)
            }
            ExprTypeError::CannotApplyUnaryOpToType {
                op_span,
                op,
                arg_span,
                arg_type,
            } => {
                let verb = op.verb();
                let message = format!("Cannot {verb} {arg_type}");
                let label = format!("this expression has type {arg_type}");
                SourceError::new(op_span, message).with_label(arg_span, label)
            }
            ExprTypeError::CannotIndexIntoType {
                bracket_span,
                indexed_span,
                indexed_type,
            } => {
                let message =
                    format!("cannot index into value of type {indexed_type}");
                let label = format!("this expression has type {indexed_type}");
                SourceError::new(bracket_span, message)
                    .with_label(indexed_span, label)
            }
            ExprTypeError::CannotUseTypeAsIndex { index_span, index_type } => {
                let message = format!("cannot use {index_type} as an index");
                let label = format!("this expression has type {index_type}");
                SourceError::new(index_span, message)
                    .with_label(index_span, label)
            }
            ExprTypeError::ListIndexStaticallyOutOfRange {
                list_span,
                list_length,
                index_span,
                index_value,
            } => {
                let message =
                    "list index is statically out of range".to_string();
                let list_label = format!("this list has length {list_length}");
                let index_label =
                    format!("the value of this expression is {index_value}");
                SourceError::new(index_span, message)
                    .with_label(list_span, list_label)
                    .with_label(index_span, index_label)
            }
            ExprTypeError::ListItemsMustAllBeSameType {
                first_item_span,
                first_item_type,
                other_item_span,
                other_item_type,
            } => {
                let message =
                    "all items in a list must have the same type".to_string();
                let label1 = format!("this item has type {first_item_type}");
                let label2 = format!("this item has type {other_item_type}");
                SourceError::new(other_item_span, message)
                    .with_label(first_item_span, label1)
                    .with_label(other_item_span, label2)
            }
            ExprTypeError::RelativeLabelInDebuggerScript { span } => {
                let message =
                    "Cannot use relative labels in a debugger script"
                        .to_string();
                SourceError::new(span, message)
            }
            ExprTypeError::RelativeLabelInLinkerConfig { span } => {
                let message = "Cannot use relative labels in a linker config"
                    .to_string();
                SourceError::new(span, message)
            }
            ExprTypeError::RelativeLabelOutsideOfAnySection { span } => {
                let message =
                    "Relative labels must be within a .SECTION".to_string();
                SourceError::new(span, message)
            }
            ExprTypeError::StaticEvalError { error } => {
                error.to_source_error()
            }
            ExprTypeError::TupleIndexNotStatic { index_span } => {
                let message = "tuple index must be static".to_string();
                let label = "this expression isn't static".to_string();
                SourceError::new(index_span, message)
                    .with_label(index_span, label)
            }
            ExprTypeError::TupleIndexOutOfRange {
                tuple_span,
                item_types,
                index_span,
                index_value,
            } => {
                let message = "tuple index out of bounds".to_string();
                let label1 = format!(
                    "this expression has type {}",
                    ExprType::Tuple(item_types)
                );
                let label2 =
                    format!("the value of this expression is {index_value}");
                SourceError::new(index_span, message)
                    .with_label(tuple_span, label1)
                    .with_label(index_span, label2)
            }
            ExprTypeError::UnknownIdentifier { span, name } => {
                let message = format!("unknown identifier: `{name}`");
                let label = "this identifier was never declared".to_string();
                SourceError::new(span, message).with_label(span, label)
            }
        }
    }
}

//===========================================================================//

/// An error encountered while evaluating an expression.
#[derive(Debug, Eq, PartialEq)]
pub enum ExprEvalError {
    /// Tried to bit shift an integer left/right by the given number of bits,
    /// but the shift amount was negative.
    BitShiftByNegative {
        /// The source code span for the right-hand side of the bit shift
        /// operation.
        rhs_span: SrcSpan,
        /// The value of the right-hand side of the bit shift operation.
        rhs_value: BigInt,
    },
    /// Tried to bit shift an integer left/right by the given number of bits,
    /// but the shift amount was too large.
    BitShiftOutOfRange {
        /// The source code span for the right-hand side of the bit shift
        /// operation.
        rhs_span: SrcSpan,
        /// The value of the right-hand side of the bit shift operation.
        rhs_value: BigUint,
    },
    /// Tried to divide an integer, but the divisor was zero.
    DivideByZero {
        /// The source code span for the right-hand side of the division
        /// operation.
        rhs_span: SrcSpan,
    },
    /// Tried to modulo an integer, but the modulus was zero.
    ModByZero {
        /// The source code span for the right-hand side of the modulo
        /// operation.
        rhs_span: SrcSpan,
    },
    /// Tried to exponentiate an integer with the given exponent, but the
    /// exponent was negative.
    PowNegativeExponent {
        /// The source code span for the right-hand side of the bit shift
        /// operation.
        rhs_span: SrcSpan,
        /// The value of the right-hand side of the bit shift operation.
        rhs_value: BigInt,
    },
    /// Tried to subtract one label from another, but the labels were in the
    /// given two different address spaces.
    SubtractLabelsInDifferentAddrspaces {
        /// The source code span for the subtraction operator.
        op_span: SrcSpan,
        /// The source code span for the left-hand side of the subtraction
        /// operation.
        lhs_span: SrcSpan,
        /// The address space of the left-hand side of the subtraction
        /// operation.
        lhs_space: Rc<str>,
        /// The source code span for the right-hand side of the subtraction
        /// operation.
        rhs_span: SrcSpan,
        /// The address space of the right-hand side of the subtraction
        /// operation.
        rhs_space: Rc<str>,
    },
    /// Could not compute the result due to an insufficiently-resolved label
    /// value.
    UnresolvedLabel {
        /// The source code span for the label.
        label_span: SrcSpan,
    },
}

impl ToSourceError for ExprEvalError {
    fn to_source_error(self) -> SourceError {
        match self {
            ExprEvalError::BitShiftByNegative { rhs_span, rhs_value } => {
                let message =
                    "cannot shift by a negative number of bits".to_string();
                let label =
                    format!("the value of this expression is {rhs_value}");
                SourceError::new(rhs_span, message).with_label(rhs_span, label)
            }
            ExprEvalError::BitShiftOutOfRange { rhs_span, rhs_value } => {
                let message = "shift by too many bits".to_string();
                let label =
                    format!("the value of this expression is {rhs_value}");
                SourceError::new(rhs_span, message).with_label(rhs_span, label)
            }
            ExprEvalError::DivideByZero { rhs_span } => {
                let message = "cannot divide by zero".to_string();
                let label = "the value of this expression is 0".to_string();
                SourceError::new(rhs_span, message).with_label(rhs_span, label)
            }
            ExprEvalError::ModByZero { rhs_span } => {
                let message = "cannot modulo by zero".to_string();
                let label = "the value of this expression is 0".to_string();
                SourceError::new(rhs_span, message).with_label(rhs_span, label)
            }
            ExprEvalError::PowNegativeExponent { rhs_span, rhs_value } => {
                let message = "exponent must be non-negative".to_string();
                let label =
                    format!("the value of this expression is {rhs_value}");
                SourceError::new(rhs_span, message).with_label(rhs_span, label)
            }
            ExprEvalError::SubtractLabelsInDifferentAddrspaces {
                op_span,
                lhs_span,
                lhs_space,
                rhs_span,
                rhs_space,
            } => {
                let message =
                    "cannot subtract labels in different address spaces"
                        .to_string();
                let lhs_label =
                    format!("this label is in address space {lhs_space}");
                let rhs_label =
                    format!("this label is in address space {rhs_space}");
                SourceError::new(op_span, message)
                    .with_label(lhs_span, lhs_label)
                    .with_label(rhs_span, rhs_label)
            }
            ExprEvalError::UnresolvedLabel { label_span } => {
                let message = "could not resolve label".to_string();
                SourceError::new(label_span, message)
            }
        }
    }
}

//===========================================================================//
