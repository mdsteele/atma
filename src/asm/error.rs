use crate::addr::{Align, AlignTryFromError};
use crate::error::{Errs, SourceError, SrcCacheError, SrcSpan, ToSourceError};
use crate::expr::{ExprType, ExprTypeError};
use crate::parse::ParseError;
use num_bigint::BigInt;
use std::range::RangeInclusive;
use std::rc::Rc;

//===========================================================================//

/// A specialized `Result` type for compiling assembly code.
pub type AsmResult<T> = Result<T, Errs<AsmError>>;

//===========================================================================//

/// An error encountered while compiling assembly code.
#[derive(Debug)]
pub enum AsmError {
    /// Tried to use an endianness-dependent directive with an architecture
    /// that has no native endianness.
    ArchHasNoEndianness {
        /// The directive name (e.g. `".U16"`).
        directive: &'static str,
        /// The source code span for the directive.
        span: SrcSpan,
        /// The name of the architecture.
        arch: Rc<str>,
    },
    /// An assertion failed statically (without needing to wait for linking).
    AssertionStaticallyFailed {
        /// The source code span for the assertion condition expression that
        /// evaluated to false.
        condition_span: SrcSpan,
        /// The additional message value for the assertion, if any.
        additional_message: Option<Rc<str>>,
    },
    /// Tried to declare something using a built-in identifier.
    DeclNameIsBuiltin {
        /// The source code span for the identifier that we tried to declare.
        span: SrcSpan,
        /// The name of the identifier that we tried to declare.
        name: Rc<str>,
    },
    /// A static directive attribute had a non-static expression.
    DirectiveExprNotStatic {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The component of the directive that this expression is used for
        /// (e.g. `"name"`).
        component: &'static str,
        /// The source code span for the non-static expression.
        expr_span: SrcSpan,
    },
    /// An directive was given an integer expression whose value was statically
    /// out of range.
    DirectiveExprOutOfRange {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The component of the directive that this expression is used for
        /// (e.g. `"name"`).
        component: &'static str,
        /// The source code span for the expression.
        expr_span: SrcSpan,
        /// The value of the expression.
        expr_value: BigInt,
        /// The range that the expression value must be within.
        valid_range: RangeInclusive<BigInt>,
    },
    /// A directive was given an expression with the wrong type.
    DirectiveExprTypeError {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The component of the directive that this expression is used for
        /// (e.g. `"name"`).
        component: &'static str,
        /// The source code span for the expresion.
        expr_span: SrcSpan,
        /// The actual type of the expression.
        expr_type: ExprType,
        /// The permissible types for the expression.
        valid_types: Vec<ExprType>,
    },
    /// A directive (or label) that must be in a `.SECTION` was found outside
    /// of any `.SECTION`.
    DirectiveNotInSection {
        /// The directive name (e.g. `".SECTION"`), or "label".
        directive: &'static str,
        /// The source code span for the directive or label.
        span: SrcSpan,
    },
    /// A directive was given two attributes with the same name.
    DuplicateAttrName {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The duplicated attribute name.
        attr_name: Rc<str>,
        /// The source code span for the duplicate instance of this attribute
        /// name.
        attr_span: SrcSpan,
        /// The source code span for the earlier instance of this attribute
        /// name.
        prev_span: SrcSpan,
    },
    /// A macro definnition included two placeholders with the same name.
    DuplicateMacroPlaceholder {
        /// The duplicated placeholder name.
        placeholder_name: Rc<str>,
        /// The source code span for the duplicate instance of this placeholder
        /// name.
        placeholder_span: SrcSpan,
        /// The source code span for the earlier instance of this placeholder
        /// name.
        prev_span: SrcSpan,
    },
    /// An expression failed to typecheck.
    ExprTypeError {
        /// The typechecking error.
        error: ExprTypeError,
    },
    /// An alignment value had an invalid value.
    InvalidAlignmentValue {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The attribute name.
        attr_name: &'static str,
        /// The reason that the expression value was invalid.
        error: AlignTryFromError,
        /// The source code span for the expression that evaluated to an
        /// invalid alignment value.
        expr_span: SrcSpan,
        /// The value of the expression.
        expr_value: BigInt,
    },
    /// A directive was given an unknown attribute name.
    InvalidAttrName {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The unknkown attribute name.
        attr_name: Rc<str>,
        /// The source code span for the attribute name.
        attr_span: SrcSpan,
    },
    /// A unicode scalar value expression had an invalid value.
    InvalidUnicodeScalarValue {
        /// The source code span for the expression that evaluated to an
        /// invalid unicode scalar value.
        expr_span: SrcSpan,
        /// The value of the expression.
        expr_value: BigInt,
    },
    /// A macro definition included multiple placeholders in a single macro
    /// parameter.
    MultipleMacroPlaceholders {
        // TODO: add more error details
        /// The source code span for the macro parameter.
        span: SrcSpan,
    },
    /// An piece of assembly source code failed to parse.
    ParseError {
        /// The parse error.
        error: ParseError,
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
    /// Tried to declare a symbol that had already been declared.
    SymbolAlreadyDeclared {
        /// The fully-qualified name of the symbol.
        full_name: Rc<str>,
        /// The source code span for the duplicate declaration of the symbol.
        name_span: SrcSpan,
        /// The source code span for the earlier declaration of the symbol.
        prev_span: SrcSpan,
    },
    /// Tried to switch to an architecture that was never defined.
    UnknownArch {
        /// The name of the undefined architecture.
        arch: Rc<str>,
        /// The source code span for the expression that evaluated to the
        /// unknown architecture name.
        span: SrcSpan,
    },
    /// Tried to use an undeclared placeholder in a macro definition.
    UnknownMacroPlaceholder {
        /// The name of the undefined placeholder.
        name: Rc<str>,
        /// The source code span for the placeholder.
        span: SrcSpan,
    },
    /// Found a macro invocation with no matching macro definition.
    UnmatchedMacroInvocation {
        /// The name of the macro.
        macro_name: Rc<str>,
        /// The name of the current architecture.
        arch: Rc<str>,
        /// The source code span for the macro invocation.
        invocation_span: SrcSpan,
    },
}

impl ToSourceError for AsmError {
    fn to_source_error(self) -> SourceError {
        match self {
            Self::ArchHasNoEndianness { directive, span, arch } => {
                let message = format!(
                    "Cannot use {directive} under architecture {arch:?}, \
                     which has no defined endianness"
                );
                SourceError::new(span, message)
            }
            Self::AssertionStaticallyFailed {
                condition_span,
                additional_message,
            } => {
                let message = if let Some(additional) = additional_message {
                    format!("Assertion failed: {additional}")
                } else {
                    "Assertion failed".to_string()
                };
                SourceError::new(condition_span, message)
            }
            Self::DeclNameIsBuiltin { span, name } => {
                let message = format!(
                    "cannot declare `{name}`; identifiers starting with `%` \
                     are reserved"
                );
                SourceError::new(span, message)
            }
            Self::DirectiveExprNotStatic {
                directive,
                component,
                expr_span,
            } => {
                let message =
                    format!("{directive} {component} must be static");
                let label = "this expression isn't static";
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::DirectiveExprOutOfRange {
                directive,
                component,
                expr_span,
                expr_value,
                valid_range,
            } => {
                let message = format!(
                    "{directive} {component} must be between {} and {}",
                    valid_range.start, valid_range.last
                );
                let label =
                    format!("the value of this expression is {expr_value}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::DirectiveExprTypeError {
                directive,
                component,
                expr_span,
                expr_type,
                valid_types,
            } => {
                let message = format!(
                    "{directive} {component} must have type {}",
                    valid_types
                        .iter()
                        .map(ExprType::to_string)
                        .collect::<Vec<_>>()
                        .join(" or "),
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::DirectiveNotInSection { directive, span } => {
                let message = format!("{directive} must be within a .SECTION");
                SourceError::new(span, message)
            }
            Self::DuplicateAttrName {
                directive,
                attr_name,
                attr_span,
                prev_span,
            } => {
                let message = format!(
                    "Duplicate `{attr_name}` attribute for {directive}"
                );
                let label1 = "Previously declared here";
                let label2 = "Duplicated here";
                SourceError::new(attr_span, message)
                    .with_label(prev_span, label1)
                    .with_label(attr_span, label2)
            }
            Self::DuplicateMacroPlaceholder {
                placeholder_name,
                placeholder_span,
                prev_span,
            } => {
                let message = format!(
                    "Duplicate `{placeholder_name}` macro placeholder"
                );
                let label1 = "Previously declared here";
                let label2 = "Duplicated here";
                SourceError::new(placeholder_span, message)
                    .with_label(prev_span, label1)
                    .with_label(placeholder_span, label2)
            }
            Self::ExprTypeError { error } => error.to_source_error(),
            Self::InvalidAlignmentValue {
                directive,
                attr_name,
                error,
                expr_span,
                expr_value,
            } => {
                let message = match error {
                    AlignTryFromError::NotAPowerOfTwo => {
                        format!(
                            "{directive} `{attr_name}` attribute must be a \
                             power of two"
                        )
                    }
                    AlignTryFromError::TooLargePowerOfTwo => {
                        format!(
                            "{directive} `{attr_name}` attribute must be at \
                             most ${:x}",
                            Align::MAX
                        )
                    }
                };
                let label =
                    format!("the value of this expression is ${expr_value:x}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::InvalidAttrName { directive, attr_name, attr_span } => {
                let message =
                    format!("Invalid {directive} attribute: `{attr_name}`");
                SourceError::new(attr_span, message)
            }
            Self::InvalidUnicodeScalarValue { expr_span, expr_value } => {
                let message = "invalid unicode scalar value";
                let label =
                    format!("the value of this expression is {expr_value}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::MultipleMacroPlaceholders { span } => {
                let message = "multiple placeholders";
                SourceError::new(span, message)
            }
            Self::ParseError { error } => error.to_source_error(),
            Self::SrcCacheError { path, path_span, error } => {
                let message = format!("error loading {path:?}: {error}");
                SourceError::new(path_span, message)
            }
            Self::SymbolAlreadyDeclared {
                full_name,
                name_span,
                prev_span,
            } => {
                let message =
                    format!("symbol was already declared: {}", full_name);
                let label1 = "previously declared here";
                let label2 = "redeclared here";
                SourceError::new(name_span, message)
                    .with_label(prev_span, label1)
                    .with_label(name_span, label2)
            }
            Self::UnknownArch { arch, span } => {
                let message =
                    format!("the `{arch}` architecture was never defined");
                let label =
                    format!("The value of this expression is {arch:?}");
                SourceError::new(span, message).with_label(span, label)
            }
            Self::UnknownMacroPlaceholder { name, span } => {
                let message = format!("Undeclared placeholder: `{name}`");
                SourceError::new(span, message)
            }
            Self::UnmatchedMacroInvocation {
                macro_name,
                arch,
                invocation_span,
            } => {
                let message = format!(
                    "no match for `{macro_name}` in architecture `{arch}`"
                );
                SourceError::new(invocation_span, message)
            }
        }
    }
}

//===========================================================================//
