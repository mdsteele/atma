use crate::error::{Errs, SourceError, SrcCacheError, SrcSpan, ToSourceError};
use crate::expr::{ExprType, ExprTypeError};
use crate::parse::ParseError;
use std::rc::Rc;

//===========================================================================//

/// A specialized `Result` type for compiling ADS code.
pub type AdsResult<T> = Result<T, Errs<AdsError>>;

//===========================================================================//

/// An error encountered while compiling ADS code.
#[derive(Debug)]
pub enum AdsError {
    /// Tried to modify a constant.
    CannotModifyConstant {
        /// The name of the constant.
        name: Rc<str>,
        /// The source code span where the constant was used as an lvalue.
        lvalue_span: SrcSpan,
        /// The source code span for the constant's declaration.
        decl_span: SrcSpan,
    },
    /// An expression failed to typecheck.
    ExprTypeError {
        /// The typechecking error.
        error: ExprTypeError,
    },
    /// A memory address was specified using an expression of the wrong
    /// type.
    MemoryAddrTypeError {
        /// The source code span for the memory address expression.
        expr_span: SrcSpan,
        /// The type of the expression.
        expr_type: ExprType,
    },
    /// An piece of ADS source code failed to parse.
    ParseError {
        /// The parse error.
        error: ParseError,
    },
    /// Tried to use a file path expression that wasn't static.
    PathNotStatic {
        /// The source code span for the non-static expression.
        expr_span: SrcSpan,
    },
    /// A source code path was specified using a non-string expression.
    PathTypeError {
        /// The source code span for the non-static expression.
        expr_span: SrcSpan,
        /// The type of the expression.
        expr_type: ExprType,
    },
    /// A control flow predicate was specified using a non-boolean expression.
    PredicateTypeError {
        /// The source code span for the predicate expression.
        expr_span: SrcSpan,
        /// The type of the expression.
        expr_type: ExprType,
    },
    /// Encountered an error while trying to fetch data from a file.
    SrcCacheError {
        /// The joined path for the source file that couldn't be fetched.
        path: Rc<str>,
        /// The source code span for the expression that determined the file to
        /// be fetched.
        path_span: SrcSpan,
        /// The error from the source cache.
        error: SrcCacheError,
    },
    /// Tried to modify a variable that was never declared.
    UnknownVariable {
        /// The name of the undeclared variable.
        name: Rc<str>,
        /// The source code span for the unknown variable name.
        span: SrcSpan,
    },
    /// Tried to assign an expression of one type to an lvalue of a different
    /// type.
    VariableTypeError {
        /// The source code span for the right-hand expression.
        expr_span: SrcSpan,
        /// The type of the expression.
        expr_type: ExprType,
        /// The source code span for the lvalue.
        lvalue_span: SrcSpan,
        /// The type of the lvalue.
        lvalue_type: ExprType,
    },
}

impl From<ExprTypeError> for AdsError {
    fn from(error: ExprTypeError) -> Self {
        Self::ExprTypeError { error }
    }
}

impl From<ParseError> for AdsError {
    fn from(error: ParseError) -> Self {
        Self::ParseError { error }
    }
}

impl ToSourceError for AdsError {
    fn to_source_error(self) -> SourceError {
        match self {
            Self::CannotModifyConstant { name, lvalue_span, decl_span } => {
                let message =
                    format!("cannot change value of constant `{name}`");
                let label1 =
                    format!("`{name}` was declared as a constant here");
                let label2 = format!("cannot set value of `{name}` here");
                SourceError::new(lvalue_span, message)
                    .with_label(decl_span, label1)
                    .with_label(lvalue_span, label2)
            }
            Self::ExprTypeError { error } => error.to_source_error(),
            Self::MemoryAddrTypeError { expr_span, expr_type } => {
                // TODO: Allow `ExprType::Label` as well.
                let message = format!(
                    "memory address must be of type {}, not {expr_type}",
                    ExprType::Integer
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::ParseError { error } => error.to_source_error(),
            Self::PathNotStatic { expr_span } => {
                let message = "source code path must be static";
                let label = "this expression isn't static";
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::PathTypeError { expr_span, expr_type } => {
                let message = format!(
                    "source code path must be of type {}, not {expr_type}",
                    ExprType::String
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::PredicateTypeError { expr_span, expr_type } => {
                let message = format!(
                    "predicate must be of type {}, not {expr_type}",
                    ExprType::Boolean
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::SrcCacheError { path, path_span, error } => {
                let message = format!("error loading {path:?}: {error}");
                SourceError::new(path_span, message)
            }
            Self::UnknownVariable { name, span } => {
                let message = format!("no such variable: `{name}`");
                let label = "this was never declared";
                SourceError::new(span, message).with_label(span, label)
            }
            Self::VariableTypeError {
                expr_span,
                expr_type,
                lvalue_span,
                lvalue_type,
            } => {
                let message = format!(
                    "cannot assign {expr_type} value to {lvalue_type} \
                     destination"
                );
                let label1 = format!("this expression has type {expr_type}");
                let label2 =
                    format!("this destination has type {lvalue_type}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label1)
                    .with_label(lvalue_span, label2)
            }
        }
    }
}

//===========================================================================//
