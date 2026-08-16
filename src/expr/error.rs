use super::value::{ExprType, ExprValue};
use crate::error::{Errs, SourceContext, SourceError, SrcLoc, SrcSpan};
use crate::parse::{BinOpAst, UnOpAst};
use num_bigint::{BigInt, BigUint};
use std::rc::Rc;

//===========================================================================//

pub(crate) type ExprStatic = Result<ExprValue, ExprNotStaticReason>;

/// A specialized `Result` type for expression typechecking.
pub type ExprTypeResult<T> = Result<T, Errs<ExprTypeError>>;

//===========================================================================//

/// An error encountered while typechecking an expression.
#[derive(Debug)]
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
    /// Tried to call a function with an argument of the wrong type.
    CannotCallFuncWithType {
        /// The source code span for the function expression that we tried to
        /// call.
        func_span: SrcSpan,
        /// The expression type of the function expression that we tried to
        /// call.
        func_type: ExprType,
        /// The source code span for the argument to the function.
        arg_span: SrcSpan,
        /// The actual expression type of the argument to the function.
        arg_type: ExprType,
        /// The required expression type for the function parameter.
        param_type: ExprType,
    },
    /// Tried to call a non-function as though it were a function.
    CannotCallType {
        /// The source code span for the non-function expression that we tried
        /// to call.
        func_span: SrcSpan,
        /// The expression type of the non-function expression that we tried to
        /// call.
        func_type: ExprType,
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
    /// A control flow predicate was specified using a non-boolean expression.
    CannotUseTypeAsPredicate {
        /// The source code span for the predicate expression.
        expr_span: SrcSpan,
        /// The type of the expression.
        expr_type: ExprType,
    },
    /// Found a ternary condition expression whose branches don't have the same
    /// expression type.
    ConditionBranchesMustBeSameType {
        /// The source code span for the `true` branch of the conditional.
        true_branch_span: SrcSpan,
        /// The expression type of the `true` branch of the conditional.
        true_branch_type: ExprType,
        /// The source code span for the `false` branch of the conditional.
        false_branch_span: SrcSpan,
        /// The expression type of the `false` branch of the conditional.
        false_branch_type: ExprType,
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
    /// Tried to use an identifier whose name is reserved under the current
    /// architecture.
    ReservedIdentifier {
        /// The source code span for the  identifier.
        span: SrcSpan,
        /// The name of the identifier.
        name: Rc<str>,
        /// The name of the architecture under which the name is reserved.
        arch: Rc<str>,
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
        /// The reason that the tuple index isn't static.
        reason: ExprNotStaticReason,
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

impl ExprTypeError {
    /// Converts the error into a `SourceError`, using the given path for the
    /// source file containing the expression.
    pub fn to_source_error(self, path: &Rc<str>) -> SourceError {
        match self {
            Self::CannotApplyBinaryOpToTypes {
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
                let mut error =
                    SourceError::new(SrcLoc::new(path, op_span), message)
                        .with_label(SrcLoc::new(path, lhs_span), lhs_label)
                        .with_label(SrcLoc::new(path, rhs_span), rhs_label);
                match (op, lhs_type, rhs_type) {
                    (BinOpAst::Add, ExprType::List(_), ExprType::List(_))
                    | (BinOpAst::Add, ExprType::String, ExprType::String) => {
                        error = error.with_note(
                            "To concatenate, use the `++` operator instead",
                        );
                    }
                    _ => {}
                }
                error
            }
            Self::CannotApplyUnaryOpToType {
                op_span,
                op,
                arg_span,
                arg_type,
            } => {
                let verb = op.verb();
                let message = format!("Cannot {verb} {arg_type}");
                let label = format!("this expression has type {arg_type}");
                SourceError::new(SrcLoc::new(path, op_span), message)
                    .with_label(SrcLoc::new(path, arg_span), label)
            }
            Self::CannotCallFuncWithType {
                func_span,
                func_type,
                arg_span,
                arg_type,
                param_type,
            } => {
                let message = format!(
                    "expected {param_type} argument, but found {arg_type}"
                );
                let func_label =
                    format!("this expression has type {func_type}");
                let arg_label = format!("this expression has type {arg_type}");
                SourceError::new(SrcLoc::new(path, arg_span), message)
                    .with_label(SrcLoc::new(path, func_span), func_label)
                    .with_primary_label(arg_label)
            }
            Self::CannotCallType { func_span, func_type } => {
                let message =
                    format!("cannot call non-function type {func_type}");
                let label = format!("this expression has type {func_type}");
                SourceError::new(SrcLoc::new(path, func_span), message)
                    .with_primary_label(label)
            }
            Self::CannotIndexIntoType {
                bracket_span,
                indexed_span,
                indexed_type,
            } => {
                let message =
                    format!("cannot index into value of type {indexed_type}");
                let label = format!("this expression has type {indexed_type}");
                SourceError::new(SrcLoc::new(path, bracket_span), message)
                    .with_label(SrcLoc::new(path, indexed_span), label)
            }
            Self::CannotUseTypeAsIndex { index_span, index_type } => {
                let message = format!("cannot use {index_type} as an index");
                let label = format!("this expression has type {index_type}");
                SourceError::new(SrcLoc::new(path, index_span), message)
                    .with_primary_label(label)
            }
            Self::CannotUseTypeAsPredicate { expr_span, expr_type } => {
                let message = format!(
                    "predicate must be of type {}, not {expr_type}",
                    ExprType::Boolean
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(SrcLoc::new(path, expr_span), message)
                    .with_primary_label(label)
            }
            Self::ConditionBranchesMustBeSameType {
                true_branch_span,
                true_branch_type,
                false_branch_span,
                false_branch_type,
            } => {
                let branches_span =
                    true_branch_span.merged_with(false_branch_span);
                let message =
                    "both sides of a conditional must have the same type";
                let label1 = format!("this side has type {true_branch_type}");
                let label2 = format!("this side has type {false_branch_type}");
                SourceError::new(SrcLoc::new(path, branches_span), message)
                    .with_label(SrcLoc::new(path, true_branch_span), label1)
                    .with_label(SrcLoc::new(path, false_branch_span), label2)
            }
            Self::ListItemsMustAllBeSameType {
                first_item_span,
                first_item_type,
                other_item_span,
                other_item_type,
            } => {
                let message = "all items in a list must have the same type";
                let label1 = format!("this item has type {first_item_type}");
                let label2 = format!("this item has type {other_item_type}");
                SourceError::new(SrcLoc::new(path, other_item_span), message)
                    .with_label(SrcLoc::new(path, first_item_span), label1)
                    .with_primary_label(label2)
            }
            Self::RelativeLabelInDebuggerScript { span } => {
                let message =
                    "Cannot use relative labels in a debugger script";
                SourceError::new(SrcLoc::new(path, span), message)
                    .with_primary_label("")
            }
            Self::RelativeLabelInLinkerConfig { span } => {
                let message = "Cannot use relative labels in a linker config";
                SourceError::new(SrcLoc::new(path, span), message)
                    .with_primary_label("")
            }
            Self::RelativeLabelOutsideOfAnySection { span } => {
                let message = "Relative labels must be within a .SECTION";
                SourceError::new(SrcLoc::new(path, span), message)
                    .with_primary_label("")
            }
            Self::ReservedIdentifier { span, name, arch } => {
                let message = format!(
                    "`{name}` is a reserved word under architecture {arch:?}"
                );
                SourceError::new(SrcLoc::new(path, span), message)
                    .with_primary_label("")
            }
            Self::StaticEvalError { error } => error.to_source_error(path),
            Self::TupleIndexNotStatic { index_span, reason } => {
                let message = "tuple index must be static";
                let label = "this expression isn't static";
                SourceError::new(SrcLoc::new(path, index_span), message)
                    .with_primary_label(label)
                    .with_context(&reason.context(path))
            }
            Self::TupleIndexOutOfRange {
                tuple_span,
                item_types,
                index_span,
                index_value,
            } => {
                let message = "tuple index out of bounds";
                let label1 = format!(
                    "this expression has type {}",
                    ExprType::Tuple(item_types)
                );
                let label2 =
                    format!("the value of this expression is {index_value}");
                SourceError::new(SrcLoc::new(path, index_span), message)
                    .with_label(SrcLoc::new(path, tuple_span), label1)
                    .with_primary_label(label2)
            }
            Self::UnknownIdentifier { span, name } => {
                let message = format!("unknown identifier: `{name}`");
                let label = "this identifier was never declared";
                SourceError::new(SrcLoc::new(path, span), message)
                    .with_primary_label(label)
            }
        }
    }
}

//===========================================================================//

/// An error encountered while evaluating an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprEvalError {
    /// Tried to get the address of a label, but the label has not yet been
    /// resolved and its address is not yet known.
    AddrOfLabelUnresolved {
        /// The source code span for the address-of operator.
        op_span: SrcSpan,
        /// The source code span for the argument of the address-of operation.
        arg_span: SrcSpan,
    },
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
    /// Called the built-in `%error` function with the given message string.
    ErrorMessage {
        /// The source code span for the `%error` function call expression.
        span: SrcSpan,
        /// The error message.
        message: Rc<str>,
    },
    /// Found a value of the wrong type.
    ///
    /// This shouldn't normally happen unless an object file has been
    /// corrupted, since ATMA normally performs static typechecking before
    /// evaluation.
    InvalidType {
        /// The source code span for the value expression.
        span: SrcSpan,
    },
    /// Tried to index into a list, but the index was out of bounds.
    ListIndexOutOfBounds {
        /// The source code span for the list expression.
        list_span: SrcSpan,
        /// The length of the list.
        list_length: usize,
        /// The source code span for the index expression.
        index_span: SrcSpan,
        /// The value of the index.
        index_value: BigInt,
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
    /// Tried to calculate the square root of a negative number.
    SquareRootOfNegative {
        /// The source code span for the entire square root expression.
        expr_span: SrcSpan,
        /// The source code span for the argument of the square root.
        arg_span: SrcSpan,
        /// The value of the argument of the square root.
        arg_value: BigInt,
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
    /// Tried to subtract one label from another, but the labels have not yet
    /// been resolved and the delta is not yet known.
    SubtractLabelsUnresolved {
        /// The source code span for the subtraction operator.
        op_span: SrcSpan,
        /// The source code span for the left-hand side of the subtraction
        /// operation.
        lhs_span: SrcSpan,
        /// The source code span for the right-hand side of the subtraction
        /// operation.
        rhs_span: SrcSpan,
    },
}

impl ExprEvalError {
    /// Converts the error into a `SourceError`, using the given path for the
    /// source file containing the expression.
    pub fn to_source_error(self, path: &Rc<str>) -> SourceError {
        match self {
            Self::AddrOfLabelUnresolved { op_span, arg_span } => {
                let message = "the address of this label is not yet known";
                SourceError::new(SrcLoc::new(path, op_span), message)
                    .with_label(SrcLoc::new(path, arg_span), "")
            }
            Self::BitShiftByNegative { rhs_span, rhs_value } => {
                let message = "shift distance cannot be negative";
                let label =
                    format!("the value of this expression is {rhs_value}");
                SourceError::new(SrcLoc::new(path, rhs_span), message)
                    .with_primary_label(label)
            }
            Self::BitShiftOutOfRange { rhs_span, rhs_value } => {
                let message = "shift distance cannot be too large";
                let label =
                    format!("the value of this expression is {rhs_value}");
                SourceError::new(SrcLoc::new(path, rhs_span), message)
                    .with_primary_label(label)
            }
            Self::DivideByZero { rhs_span } => {
                let message = "divisor cannot be zero";
                let label = "the value of this expression is 0";
                SourceError::new(SrcLoc::new(path, rhs_span), message)
                    .with_primary_label(label)
            }
            Self::ErrorMessage { span, message } => {
                SourceError::new(SrcLoc::new(path, span), message)
                    .with_primary_label("")
            }
            Self::ModByZero { rhs_span } => {
                let message = "modulus cannot be zero";
                let label = "the value of this expression is 0";
                SourceError::new(SrcLoc::new(path, rhs_span), message)
                    .with_primary_label(label)
            }
            Self::InvalidType { span } => {
                SourceError::new(SrcLoc::new(path, span), "invalid type")
                    .with_primary_label("")
            }
            Self::ListIndexOutOfBounds {
                list_span,
                list_length,
                index_span,
                index_value,
            } => {
                let message = "list index is out of range";
                let list_label = format!("this list has length {list_length}");
                let index_label =
                    format!("the value of this expression is {index_value}");
                SourceError::new(SrcLoc::new(path, index_span), message)
                    .with_label(SrcLoc::new(path, list_span), list_label)
                    .with_primary_label(index_label)
            }
            Self::PowNegativeExponent { rhs_span, rhs_value } => {
                let message = "exponent must be non-negative";
                let label =
                    format!("the value of this expression is {rhs_value}");
                SourceError::new(SrcLoc::new(path, rhs_span), message)
                    .with_primary_label(label)
            }
            Self::SquareRootOfNegative { expr_span, arg_span, arg_value } => {
                let message = "square root argument must be non-negative";
                let label =
                    format!("the value of this expression is {arg_value}");
                SourceError::new(SrcLoc::new(path, expr_span), message)
                    .with_label(SrcLoc::new(path, arg_span), label)
            }
            Self::SubtractLabelsInDifferentAddrspaces {
                op_span,
                lhs_span,
                lhs_space,
                rhs_span,
                rhs_space,
            } => {
                let message =
                    "cannot subtract labels in different address spaces";
                let lhs_label =
                    format!("this label is in address space {lhs_space}");
                let rhs_label =
                    format!("this label is in address space {rhs_space}");
                SourceError::new(SrcLoc::new(path, op_span), message)
                    .with_label(SrcLoc::new(path, lhs_span), lhs_label)
                    .with_label(SrcLoc::new(path, rhs_span), rhs_label)
            }
            Self::SubtractLabelsUnresolved { op_span, lhs_span, rhs_span } => {
                let message =
                    "the delta between these labels is not yet known";
                SourceError::new(SrcLoc::new(path, op_span), message)
                    .with_label(SrcLoc::new(path, lhs_span), "")
                    .with_label(SrcLoc::new(path, rhs_span), "")
            }
        }
    }
}

//===========================================================================//

/// Describes a reason why a particular expression isn't considered static.
#[derive(Clone, Debug)]
pub enum ExprNotStaticReason {
    /// No need to statically evaluate the expression, because it is an
    /// unreachable "phantom" expression that will never be evaluated and is
    /// only relevant for typechecking (e.g. the untaken branch of a
    /// conditional whose predicate boolean is statically known).
    Phantom,
    /// Cannot statically evaluate the expression because an evaluation error
    /// would occur.
    StaticEvalError {
        /// The evaluation error that would occur if the expression were to be
        /// evaluated.
        error: ExprEvalError,
    },
    /// Cannot statically evaluate the expression because the expression did
    /// not typecheck successfully.
    TypeError,
    /// Cannot statically evaluate the expression because it depends on the
    /// value of a non-static variable.
    Variable {
        /// The source code span where the variable appears in the expression.
        span: SrcSpan,
        /// The name of the variable.
        name: Rc<str>,
    },
}

impl ExprNotStaticReason {
    /// Augments `self` with the given source file path to produce a
    /// [`SourceContext`] object that can be passed to
    /// [`SourceError::with_context`].
    pub fn context(self, path: &Rc<str>) -> ExprNotStaticContext {
        ExprNotStaticContext { path: path.clone(), reason: self }
    }
}

//===========================================================================//

/// A [`SourceContext`] to explain why a particular expression isn't considered
/// static.
pub struct ExprNotStaticContext {
    path: Rc<str>,
    reason: ExprNotStaticReason,
}

impl SourceContext for ExprNotStaticContext {
    fn annotate(&self, error: SourceError) -> SourceError {
        match &self.reason {
            ExprNotStaticReason::Phantom => error,
            ExprNotStaticReason::StaticEvalError { error: eval_error } => {
                let eval_error =
                    eval_error.clone().to_source_error(&self.path);
                let error = error.with_label(
                    eval_error.loc,
                    format!("...because {}", eval_error.message),
                );
                let error =
                    eval_error.labels.into_iter().fold(error, |e, label| {
                        e.with_label(label.loc, label.message)
                    });
                eval_error
                    .notes
                    .into_iter()
                    .fold(error, |e, note| e.with_note(note))
            }
            ExprNotStaticReason::TypeError => error,
            ExprNotStaticReason::Variable { span, name } => {
                let label = format!("...because `{name}` isn't static");
                error.with_label(SrcLoc::new(&self.path, *span), label)
            }
        }
    }
}

//===========================================================================//
