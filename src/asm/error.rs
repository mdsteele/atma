use crate::addr::{Align, AlignTryFromError};
use crate::error::{
    Errs, SourceContext, SourceError, SrcCacheError, SrcLoc, SrcSpan,
};
use crate::expr::{ExprNotStaticReason, ExprType, ExprTypeError};
use crate::parse::ParseError;
use num_bigint::BigInt;
use std::range::RangeInclusive;
use std::rc::Rc;

//===========================================================================//

/// A span of byte offsets within a particular assembly source code context.
#[derive(Clone, Debug)]
pub struct AsmSrcLoc {
    /// The span of byte offsets within the context.
    pub span: SrcSpan,
    /// The file context that this location exists within.
    pub context: Rc<AsmSrcContext>,
}

impl AsmSrcLoc {
    /// Returns the primary [`SrcLoc`] for `self`, ignoring any further
    /// context.
    fn primary(&self) -> SrcLoc {
        SrcLoc { path: self.context.path.clone(), span: self.span }
    }
}

//===========================================================================//

/// A context in which an [`AsmSrcLoc`] exists.
#[derive(Debug)]
pub struct AsmSrcContext {
    /// The path for the source code file.
    pub path: Rc<str>,
    /// The parent context that gave rise to this context.
    pub parent: AsmSrcParent,
}

impl AsmSrcContext {
    pub(crate) fn root(path: Rc<str>) -> AsmSrcContext {
        AsmSrcContext { path, parent: AsmSrcParent::Root }
    }
}

impl SourceContext for AsmSrcContext {
    fn annotate(&self, mut error: SourceError) -> SourceError {
        let mut context: &AsmSrcContext = self;
        loop {
            match &context.parent {
                AsmSrcParent::Root => return error,
                AsmSrcParent::Use(loc) => {
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

/// Describes the circumstances that gave rise to a particular
/// [`AsmSrcContext`].
#[derive(Debug)]
pub enum AsmSrcParent {
    /// Indicates that the context has no parent; it is already the main
    /// assembly code file.
    Root,
    /// Indicates that the context was created by a `.USE` directive, whose
    /// path expression is at the given location.
    Use(AsmSrcLoc),
}

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
        /// The source code location for the directive.
        loc: AsmSrcLoc,
        /// The name of the architecture.
        arch: Rc<str>,
    },
    /// An assertion failed statically (without needing to wait for linking).
    AssertionStaticallyFailed {
        /// The source code location for the assertion condition expression
        /// that evaluated to false.
        condition_loc: AsmSrcLoc,
        /// The additional message value for the assertion, if any.
        additional_message: Option<Rc<str>>,
    },
    /// Tried to assign to a built-in identifier.
    AssignmentToBuiltin {
        /// The source code location for the identifier that we tried to
        /// declare or assign to.
        loc: AsmSrcLoc,
        /// The name of the identifier.
        name: Rc<str>,
    },
    /// Tried to modify a constant (or label).
    CannotModifyConstant {
        /// The name of the constant.
        name: Rc<str>,
        /// The source code location where the constant was used as an lvalue.
        lvalue_loc: AsmSrcLoc,
        /// The source code location for the constant's declaration.
        decl_loc: AsmSrcLoc,
    },
    /// A static directive attribute had a non-static expression.
    DirectiveExprNotStatic {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The component of the directive that this expression is used for
        /// (e.g. `"name"`).
        component: &'static str,
        /// The source code location for the non-static expression.
        expr_loc: AsmSrcLoc,
        /// The reason that the expression isn't static.
        reason: ExprNotStaticReason,
    },
    /// An directive was given an integer expression whose value was statically
    /// out of range.
    DirectiveExprOutOfRange {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The component of the directive that this expression is used for
        /// (e.g. `"name"`).
        component: &'static str,
        /// The source code location for the expression.
        expr_loc: AsmSrcLoc,
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
        /// The source code location for the expresion.
        expr_loc: AsmSrcLoc,
        /// The actual type of the expression.
        expr_type: ExprType,
        /// The permissible types for the expression.
        valid_types: Vec<ExprType>,
    },
    /// A directive that must be at the top level was found inside of a
    /// `.SECTION` or scope
    DirectiveNotAtTopLevel {
        /// The directive name (e.g. `".USE"`).
        directive: &'static str,
        /// The source code location for the directive.
        loc: AsmSrcLoc,
    },
    /// A directive (or label) that must be in a `.SECTION` was found outside
    /// of any `.SECTION`.
    DirectiveNotInSection {
        /// The directive name (e.g. `".SECTION"`), or "label".
        directive: &'static str,
        /// The source code location for the directive or label.
        loc: AsmSrcLoc,
    },
    /// A directive was given two attributes with the same name.
    DuplicateAttrName {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The duplicated attribute name.
        attr_name: Rc<str>,
        /// The source code location for the duplicate instance of this
        /// attribute name.
        attr_loc: AsmSrcLoc,
        /// The source code location for the earlier instance of this attribute
        /// name.
        prev_loc: AsmSrcLoc,
    },
    /// A macro definnition included two placeholders with the same name.
    DuplicateMacroPlaceholder {
        /// The duplicated placeholder name.
        placeholder_name: Rc<str>,
        /// The source code location for the duplicate instance of this
        /// placeholder name.
        placeholder_loc: AsmSrcLoc,
        /// The source code location for the earlier instance of this
        /// placeholder name.
        prev_loc: AsmSrcLoc,
    },
    /// An expression failed to typecheck.
    ExprTypeError {
        /// The context that the expression appeared within.
        context: Rc<AsmSrcContext>,
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
        /// The source code location for the expression that evaluated to an
        /// invalid alignment value.
        expr_loc: AsmSrcLoc,
        /// The value of the expression.
        expr_value: BigInt,
    },
    /// A directive was given an unknown attribute name.
    InvalidAttrName {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The unknkown attribute name.
        attr_name: Rc<str>,
        /// The source code location for the attribute name.
        attr_loc: AsmSrcLoc,
    },
    /// A unicode scalar value expression had an invalid value.
    InvalidUnicodeScalarValue {
        /// The source code location for the expression that evaluated to an
        /// invalid unicode scalar value.
        expr_loc: AsmSrcLoc,
        /// The value of the expression.
        expr_value: BigInt,
    },
    /// A macro definition included multiple placeholders in a single macro
    /// parameter.
    MultipleMacroPlaceholders {
        // TODO: add more error details
        /// The source code location for the macro parameter.
        loc: AsmSrcLoc,
    },
    /// An piece of assembly source code failed to parse.
    ParseError {
        /// The context that the parse error occurred within.
        context: Rc<AsmSrcContext>,
        /// The parse error.
        error: ParseError,
    },
    /// Encountered an error while trying to fetch data from a file.
    SrcCacheError {
        /// The joined path for the source file that couldn't be fetched.
        path: Rc<str>,
        /// The source code location for the expression that determined the
        /// file to be fetched.
        path_loc: AsmSrcLoc,
        /// The error from the source cache.
        error: SrcCacheError,
    },
    /// Tried to declare a symbol that had already been declared.
    SymbolAlreadyDeclared {
        /// The fully-qualified name of the symbol.
        full_name: Rc<str>,
        /// The source code location for the duplicate declaration of the
        /// symbol.
        name_loc: AsmSrcLoc,
        /// The source code location for the earlier declaration of the symbol.
        prev_loc: AsmSrcLoc,
    },
    /// Tried to switch to an architecture that was never defined.
    UnknownArch {
        /// The name of the undefined architecture.
        arch: Rc<str>,
        /// The source code location for the expression that evaluated to the
        /// unknown architecture name.
        loc: AsmSrcLoc,
    },
    /// Tried to use an undeclared placeholder in a macro definition.
    UnknownMacroPlaceholder {
        /// The name of the undefined placeholder.
        name: Rc<str>,
        /// The source code location for the placeholder.
        loc: AsmSrcLoc,
    },
    /// Tried to modify a variable that was never declared.
    UnknownVariable {
        /// The name of the undeclared variable.
        name: Rc<str>,
        /// The source code location for the unknown variable name.
        loc: AsmSrcLoc,
    },
    /// Found a macro invocation with no matching macro definition.
    UnmatchedMacroInvocation {
        /// The name of the macro.
        macro_name: Rc<str>,
        /// The name of the current architecture.
        arch: Rc<str>,
        /// The source code location for the macro invocation.
        invocation_loc: AsmSrcLoc,
    },
    /// Tried to assign an expression of one type to an lvalue of a different
    /// type.
    VariableTypeError {
        /// The source code location for the right-hand expression.
        expr_loc: AsmSrcLoc,
        /// The type of the expression.
        expr_type: ExprType,
        /// The source code location for the lvalue.
        lvalue_loc: AsmSrcLoc,
        /// The type of the lvalue.
        lvalue_type: ExprType,
    },
}

impl AsmError {
    /// Converts the error into a `SourceError`.
    pub fn to_source_error(self) -> SourceError {
        match self {
            Self::ArchHasNoEndianness { directive, loc, arch } => {
                let message = format!(
                    "Cannot use {directive} under architecture {arch:?}, \
                     which has no defined endianness"
                );
                SourceError::new(loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*loc.context)
            }
            Self::AssertionStaticallyFailed {
                condition_loc,
                additional_message,
            } => {
                let message = if let Some(additional) = additional_message {
                    format!("Assertion failed: {additional}")
                } else {
                    "Assertion failed".to_string()
                };
                SourceError::new(condition_loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*condition_loc.context)
            }
            Self::AssignmentToBuiltin { loc, name } => {
                let message =
                    format!("cannot assign to builtin identifier `{name}`");
                let note = "Lowercase identifiers starting with `%` are \
                            reserved for immutable builtins.";
                SourceError::new(loc.primary(), message)
                    .with_primary_label("")
                    .with_note(note)
                    .with_context(&*loc.context)
            }
            Self::CannotModifyConstant { name, lvalue_loc, decl_loc } => {
                let message =
                    format!("cannot change value of constant `{name}`");
                let label1 = format!("`{name}` was declared here");
                let label2 = format!("cannot set value of `{name}` here");
                SourceError::new(lvalue_loc.primary(), message)
                    .with_label(decl_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*lvalue_loc.context)
            }
            Self::DirectiveExprNotStatic {
                directive,
                component,
                expr_loc,
                reason,
            } => {
                let message =
                    format!("{directive} {component} must be static");
                let label = "this expression isn't static";
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&reason.context(&expr_loc.context.path))
                    .with_context(&*expr_loc.context)
            }
            Self::DirectiveExprOutOfRange {
                directive,
                component,
                expr_loc,
                expr_value,
                valid_range,
            } => {
                let message = format!(
                    "{directive} {component} must be between {} and {}",
                    valid_range.start, valid_range.last
                );
                let label =
                    format!("the value of this expression is {expr_value}");
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*expr_loc.context)
            }
            Self::DirectiveExprTypeError {
                directive,
                component,
                expr_loc,
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
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*expr_loc.context)
            }
            Self::DirectiveNotAtTopLevel { directive, loc } => {
                let message = format!("{directive} must be at the top level");
                // TODO: include label showing the chunk/scope that the
                // directive is within
                SourceError::new(loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*loc.context)
            }
            Self::DirectiveNotInSection { directive, loc } => {
                let message = format!("{directive} must be within a .SECTION");
                SourceError::new(loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*loc.context)
            }
            Self::DuplicateAttrName {
                directive,
                attr_name,
                attr_loc,
                prev_loc,
            } => {
                let message = format!(
                    "Duplicate `{attr_name}` attribute for {directive}"
                );
                let label1 = "Previously declared here";
                let label2 = "Duplicated here";
                SourceError::new(attr_loc.primary(), message)
                    .with_label(prev_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*attr_loc.context)
            }
            Self::DuplicateMacroPlaceholder {
                placeholder_name,
                placeholder_loc,
                prev_loc,
            } => {
                let message = format!(
                    "Duplicate `{placeholder_name}` macro placeholder"
                );
                let label1 = "Previously declared here";
                let label2 = "Duplicated here";
                SourceError::new(placeholder_loc.primary(), message)
                    .with_label(prev_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*placeholder_loc.context)
            }
            Self::ExprTypeError { context, error } => {
                error.to_source_error(&context.path).with_context(&*context)
            }
            Self::InvalidAlignmentValue {
                directive,
                attr_name,
                error,
                expr_loc,
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
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*expr_loc.context)
            }
            Self::InvalidAttrName { directive, attr_name, attr_loc } => {
                let message =
                    format!("Invalid {directive} attribute: `{attr_name}`");
                SourceError::new(attr_loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*attr_loc.context)
            }
            Self::InvalidUnicodeScalarValue { expr_loc, expr_value } => {
                let message = "invalid unicode scalar value";
                let label =
                    format!("the value of this expression is {expr_value}");
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*expr_loc.context)
            }
            Self::MultipleMacroPlaceholders { loc } => {
                let message = "multiple placeholders";
                SourceError::new(loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*loc.context)
            }
            Self::ParseError { context, error } => {
                error.to_source_error(&context.path).with_context(&*context)
            }
            Self::SrcCacheError { path: other, path_loc, error } => {
                let message = format!("error loading {other:?}: {error}");
                SourceError::new(path_loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*path_loc.context)
            }
            Self::SymbolAlreadyDeclared { full_name, name_loc, prev_loc } => {
                let message =
                    format!("symbol `{full_name}` was already declared");
                let label1 = "previously declared here";
                let label2 = "redeclared here";
                SourceError::new(name_loc.primary(), message)
                    .with_label(prev_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*name_loc.context)
            }
            Self::UnknownArch { arch, loc } => {
                let message =
                    format!("the `{arch}` architecture was never defined");
                let label =
                    format!("The value of this expression is {arch:?}");
                SourceError::new(loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*loc.context)
            }
            Self::UnknownMacroPlaceholder { name, loc } => {
                let message = format!("Undeclared placeholder: `{name}`");
                SourceError::new(loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*loc.context)
            }
            Self::UnknownVariable { name, loc } => {
                let message = format!("no such variable: `{name}`");
                let label = "this was never declared";
                SourceError::new(loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*loc.context)
            }
            Self::UnmatchedMacroInvocation {
                macro_name,
                arch,
                invocation_loc,
            } => {
                let message = format!(
                    "no match for `{macro_name}` in architecture `{arch}`"
                );
                SourceError::new(invocation_loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*invocation_loc.context)
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
                    .with_primary_label(label1)
                    .with_label(lvalue_loc.primary(), label2)
                    .with_context(&*lvalue_loc.context)
            }
        }
    }
}

//===========================================================================//
