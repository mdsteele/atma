use crate::error::{
    Errs, SourceContext, SourceError, SrcCacheError, SrcLoc, SrcSpan,
};
use crate::expr::{ExprType, ExprTypeError};
use crate::parse::ParseError;
use std::rc::Rc;

//===========================================================================//

/// A span of byte offsets within a particular ADS source code context.
#[derive(Debug)]
pub struct AdsSrcLoc {
    /// The span of byte offsets within the context.
    pub span: SrcSpan,
    /// The file context that this location exists within.
    pub context: Rc<AdsSrcContext>,
}

impl AdsSrcLoc {
    /// Returns the primary [`SrcLoc`] for `self`, ignoring any further
    /// context.
    fn primary(&self) -> SrcLoc {
        SrcLoc { path: self.context.path.clone(), span: self.span }
    }
}

//===========================================================================//

/// A context in which an [`AdsSrcLoc`] exists.
#[derive(Debug)]
pub struct AdsSrcContext {
    /// The path for the source code file.
    pub path: Rc<str>,
    /// The parent context that gave rise to this context.
    pub parent: AdsSrcParent,
}

impl AdsSrcContext {
    pub(crate) fn root(path: Rc<str>) -> AdsSrcContext {
        AdsSrcContext { path, parent: AdsSrcParent::Root }
    }
}

impl SourceContext for AdsSrcContext {
    fn annotate(&self, mut error: SourceError) -> SourceError {
        let mut context: &AdsSrcContext = self;
        loop {
            match &context.parent {
                AdsSrcParent::Root => return error,
                AdsSrcParent::Use(loc) => {
                    let label =
                        format!("Included `{}` from here", context.path);
                    error = error.with_label(
                        SrcLoc::new(&loc.context.path, loc.span),
                        label,
                    );
                    context = &loc.context;
                }
            }
        }
    }
}

//===========================================================================//

/// Describe the circumstances that gave rise to a particular
/// [`AdsSrcContext`].
#[derive(Debug)]
pub enum AdsSrcParent {
    /// Indicates that the context has no parent; it is already the main ADS
    /// script file.
    Root,
    /// Indicates that the context was created by a `use` statement, whose path
    /// expression is at the given location.
    Use(AdsSrcLoc),
}

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
        /// The source code location where the constant was used as an lvalue.
        lvalue_loc: AdsSrcLoc,
        /// The source code location for the constant's declaration.
        decl_loc: AdsSrcLoc,
    },
    /// An expression failed to typecheck.
    ExprTypeError {
        /// The context that the expression appeared within.
        context: Rc<AdsSrcContext>,
        /// The typechecking error.
        error: ExprTypeError,
    },
    /// A memory address was specified using an expression of the wrong
    /// type.
    MemoryAddrTypeError {
        /// The source code location for the memory address expression.
        expr_loc: AdsSrcLoc,
        /// The type of the expression.
        expr_type: ExprType,
    },
    /// An piece of ADS source code failed to parse.
    ParseError {
        /// The context that the parse error occurred within.
        context: Rc<AdsSrcContext>,
        /// The parse error.
        error: ParseError,
    },
    /// Tried to use a file path expression that wasn't static.
    PathNotStatic {
        /// The source code location for the non-static expression.
        expr_loc: AdsSrcLoc,
    },
    /// A source code path was specified using a non-string expression.
    PathTypeError {
        /// The source code location for the non-static expression.
        expr_loc: AdsSrcLoc,
        /// The type of the expression.
        expr_type: ExprType,
    },
    /// A control flow predicate was specified using a non-boolean expression.
    PredicateTypeError {
        /// The source code location for the predicate expression.
        expr_loc: AdsSrcLoc,
        /// The type of the expression.
        expr_type: ExprType,
    },
    /// Encountered an error while trying to fetch data from a file.
    SrcCacheError {
        /// The joined path for the source file that couldn't be fetched.
        path: Rc<str>,
        /// The source code location for the expression that determined the
        /// file to be fetched.
        path_loc: AdsSrcLoc,
        /// The error from the source cache.
        error: SrcCacheError,
    },
    /// Tried to modify a variable that was never declared.
    UnknownVariable {
        /// The name of the undeclared variable.
        name: Rc<str>,
        /// The source code location for the unknown variable name.
        loc: AdsSrcLoc,
    },
    /// Tried to assign an expression of one type to an lvalue of a different
    /// type.
    VariableTypeError {
        /// The source code location for the right-hand expression.
        expr_loc: AdsSrcLoc,
        /// The type of the expression.
        expr_type: ExprType,
        /// The source code location for the lvalue.
        lvalue_loc: AdsSrcLoc,
        /// The type of the lvalue.
        lvalue_type: ExprType,
    },
}

impl AdsError {
    /// Converts the error into a `SourceError`.
    pub fn to_source_error(self) -> SourceError {
        match self {
            Self::CannotModifyConstant { name, lvalue_loc, decl_loc } => {
                let message =
                    format!("cannot change value of constant `{name}`");
                let label1 =
                    format!("`{name}` was declared as a constant here");
                let label2 = format!("cannot set value of `{name}` here");
                SourceError::new(lvalue_loc.primary(), message)
                    .with_label(decl_loc.primary(), label1)
                    .with_label(lvalue_loc.primary(), label2)
                    .with_context(&*lvalue_loc.context)
            }
            Self::ExprTypeError { error, context } => {
                error.to_source_error(&context.path).with_context(&*context)
            }
            Self::MemoryAddrTypeError { expr_loc, expr_type } => {
                // TODO: Allow `ExprType::Label` as well.
                let message = format!(
                    "memory address must be of type {}, not {expr_type}",
                    ExprType::Integer
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(expr_loc.primary(), message)
                    .with_label(expr_loc.primary(), label)
                    .with_context(&*expr_loc.context)
            }
            Self::ParseError { error, context } => {
                error.to_source_error(&context.path).with_context(&*context)
            }
            Self::PathNotStatic { expr_loc } => {
                let message = "source code path must be static";
                let label = "this expression isn't static";
                SourceError::new(expr_loc.primary(), message)
                    .with_label(expr_loc.primary(), label)
                    .with_context(&*expr_loc.context)
            }
            Self::PathTypeError { expr_loc, expr_type } => {
                let message = format!(
                    "source code path must be of type {}, not {expr_type}",
                    ExprType::String
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(expr_loc.primary(), message)
                    .with_label(expr_loc.primary(), label)
                    .with_context(&*expr_loc.context)
            }
            Self::PredicateTypeError { expr_loc, expr_type } => {
                let message = format!(
                    "predicate must be of type {}, not {expr_type}",
                    ExprType::Boolean
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(expr_loc.primary(), message)
                    .with_label(expr_loc.primary(), label)
                    .with_context(&*expr_loc.context)
            }
            Self::SrcCacheError { path, path_loc, error } => {
                let message = format!("error loading {path:?}: {error}");
                let label = format!("Tried to load {path:?} here");
                SourceError::new(path_loc.primary(), message)
                    .with_label(path_loc.primary(), label)
                    .with_context(&*path_loc.context)
            }
            Self::UnknownVariable { name, loc } => {
                let message = format!("no such variable: `{name}`");
                let label = "this was never declared";
                SourceError::new(loc.primary(), message)
                    .with_label(loc.primary(), label)
                    .with_context(&*loc.context)
            }
            Self::VariableTypeError {
                expr_loc,
                expr_type,
                lvalue_loc,
                lvalue_type,
            } => {
                let message = format!(
                    "cannot assign {expr_type} value to {lvalue_type} \
                     destination"
                );
                let label1 = format!("this expression has type {expr_type}");
                let label2 =
                    format!("this destination has type {lvalue_type}");
                SourceError::new(expr_loc.primary(), message)
                    .with_label(expr_loc.primary(), label1)
                    .with_label(lvalue_loc.primary(), label2)
                    .with_context(&*lvalue_loc.context)
            }
        }
    }
}

//===========================================================================//
