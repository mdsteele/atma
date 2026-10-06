use crate::addr::{Align, AlignTryFromError};
use crate::error::{Errs, SourceError, SrcCacheError};
use crate::expr::{
    ExprEvalError, ExprNotStaticReason, ExprType, ExprTypeError,
};
use crate::obj::{ObjSrcContext, ObjSrcLoc};
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
        /// The source code location for the directive.
        loc: ObjSrcLoc,
        /// The name of the architecture.
        arch: Rc<str>,
    },
    /// Tried to assign to a built-in identifier.
    AssignmentToBuiltin {
        /// The source code location for the identifier that we tried to
        /// declare or assign to.
        loc: ObjSrcLoc,
        /// The name of the identifier.
        name: Rc<str>,
    },
    /// Tried to modify a constant (or label).
    CannotModifyConstant {
        /// The name of the constant.
        name: Rc<str>,
        /// The source code location where the constant was used as an lvalue.
        lvalue_loc: ObjSrcLoc,
        /// The source code location for the constant's declaration.
        decl_loc: ObjSrcLoc,
    },
    /// Tried to declare a charmap with the same name as an existing charmap.
    CharmapAlreadyDefined {
        /// The name of the charmap.
        name: Rc<str>,
        /// The source code location for the duplicate charmap name.
        name_loc: ObjSrcLoc,
        /// The source code location for the earlier declaration of a charmap
        /// with the same name.
        prev_loc: ObjSrcLoc,
    },
    /// Declared a mapping key in a charmap that conflicts with an earlier key.
    CharmapKeyConflict {
        /// The source code location for the conflicting key.
        key_loc: ObjSrcLoc,
        /// The key string that is in conflict with the earlier key.
        key_string: Rc<str>,
        /// The source code location for the earlier key.
        prev_loc: ObjSrcLoc,
        /// The earlier key string that the new one is in conflict with.
        prev_string: Rc<str>,
    },
    /// Declared a charmap mapping with an invalid pair of types.
    CharmapMappingTypeError {
        /// The source code location for the expression to map from.
        from_loc: ObjSrcLoc,
        /// The type of the expression to map from.
        from_type: ExprType,
        /// The source code location for the expression to map to.
        to_loc: ObjSrcLoc,
        /// The type of the expression to map to.
        to_type: ExprType,
    },
    /// Declared a charmap range mapping where the lower bound of the byte
    /// range was greater than the upper bound.
    CharmapRangeMappingEmptyByteRange {
        /// The source code location for the byte range.
        range_loc: ObjSrcLoc,
        /// The lower bound of the byte range.
        range_start: u8,
        /// The upper bound of the byte range.
        range_last: u8,
    },
    /// Declared a charmap range mapping where the lower bound of the character
    /// range was greater than the upper bound.
    CharmapRangeMappingEmptyCharRange {
        /// The source code location for the character range.
        range_loc: ObjSrcLoc,
        /// The lower bound of the character range.
        range_start: char,
        /// The upper bound of the character range.
        range_last: char,
    },
    /// Declared a charmap range mapping where one of the endpoints of the
    /// character range wasn't a single character string.
    CharmapRangeMappingInvalidCharEndpoint {
        /// The source code location for the character range.
        range_loc: ObjSrcLoc,
        /// The endpoint string that doesn't consist of exactly one character.
        range_endpoint: Rc<str>,
    },
    /// Declared a charmap range mapping where the two ranges don't have the
    /// same length.
    CharmapRangeMappingWithUnequalLengths {
        /// The source code location for the character range.
        char_range_loc: ObjSrcLoc,
        /// The number of values in the character range.
        char_range_len: u32,
        /// The source code location for the byte range.
        byte_range_loc: ObjSrcLoc,
        /// The number of values in the byte range.
        byte_range_len: u32,
    },
    /// A static directive attribute had a non-static expression.
    DirectiveExprNotStatic {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The component of the directive that this expression is used for
        /// (e.g. `"name"`).
        component: &'static str,
        /// The source code location for the non-static expression.
        expr_loc: ObjSrcLoc,
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
        expr_loc: ObjSrcLoc,
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
        expr_loc: ObjSrcLoc,
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
        loc: ObjSrcLoc,
    },
    /// A directive (or label) that must be in a `.SECTION` was found outside
    /// of any `.SECTION`.
    DirectiveNotInSection {
        /// The directive name (e.g. `".SECTION"`), or "label".
        directive: &'static str,
        /// The source code location for the directive or label.
        loc: ObjSrcLoc,
    },
    /// A directive was given two attributes with the same name.
    DuplicateAttrName {
        /// The directive name (e.g. `".SECTION"`).
        directive: &'static str,
        /// The duplicated attribute name.
        attr_name: Rc<str>,
        /// The source code location for the duplicate instance of this
        /// attribute name.
        attr_loc: ObjSrcLoc,
        /// The source code location for the earlier instance of this attribute
        /// name.
        prev_loc: ObjSrcLoc,
    },
    /// A macro definnition included two placeholders with the same name.
    DuplicateMacroPlaceholder {
        /// The name of the macro.
        macro_name: Rc<str>,
        /// The duplicated placeholder name.
        placeholder_name: Rc<str>,
        /// The source code location for the duplicate instance of this
        /// placeholder name.
        placeholder_loc: ObjSrcLoc,
        /// The source code location for the earlier instance of this
        /// placeholder name.
        prev_loc: ObjSrcLoc,
    },
    /// A struct definnition included two fields with the same name.
    DuplicateStructField {
        /// The name of the struct.
        struct_name: Rc<str>,
        /// The duplicated field name.
        field_name: Rc<str>,
        /// The source code location for the duplicate instance of this field
        /// name.
        field_loc: ObjSrcLoc,
        /// The source code location for the earlier instance of this field
        /// name.
        prev_loc: ObjSrcLoc,
    },
    /// An expression failed to typecheck.
    ExprTypeError {
        /// The context that the expression appeared within.
        context: Rc<ObjSrcContext>,
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
        expr_loc: ObjSrcLoc,
        /// The value of the expression.
        expr_value: BigInt,
    },
    /// An `.ASCII` directive had a non-ASCII string or character value.
    InvalidAsciiString {
        /// The source code location for the expression.
        expr_loc: ObjSrcLoc,
        /// The non-ASCII character that was found.
        non_ascii_char: char,
    },
    /// An ASCII byte value expression had an invalid value.
    InvalidAsciiValue {
        /// The source code location for the expression that evaluated to an
        /// invalid ASCII value.
        expr_loc: ObjSrcLoc,
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
        attr_loc: ObjSrcLoc,
    },
    /// A unicode scalar value expression had an invalid value.
    InvalidUnicodeScalarValue {
        /// The source code location for the expression that evaluated to an
        /// invalid unicode scalar value.
        expr_loc: ObjSrcLoc,
        /// The value of the expression.
        expr_value: BigInt,
    },
    /// A macro definition included multiple placeholders in a single macro
    /// parameter.
    MultipleMacroPlaceholders {
        // TODO: add more error details
        /// The source code location for the macro parameter.
        loc: ObjSrcLoc,
    },
    /// Tried to declare a name that conflicts with an existing declaration.
    NameAlreadyDeclared {
        /// The fully-qualified name.
        full_name: Rc<str>,
        /// The source code location for the duplicate declaration of the
        /// symbol.
        name_loc: ObjSrcLoc,
        /// The source code location for the earlier declaration of the symbol.
        prev_loc: ObjSrcLoc,
    },
    /// A .REPEAT directive had a negative repeat count.
    NegativeRepeatCount {
        /// The source code location for the repeat count expression.
        expr_loc: ObjSrcLoc,
        /// The value of the expression.
        expr_value: BigInt,
    },
    /// Tried to translate a string using the current charmap, but no current
    /// charmap is set.
    NoCharmapSet {
        /// The source code location for the string we tried to translate.
        expr_loc: ObjSrcLoc,
    },
    /// Tried to translate a string using the current charmap, but encountered
    /// a substring with with no matching mapping.
    NoMatchingCharmapMapping {
        /// The name of the current charmap.
        charmap: Rc<str>,
        /// The source code location for the string we tried to translate.
        expr_loc: ObjSrcLoc,
        /// The unmatched portion of the string.
        unmatched: Rc<str>,
    },
    /// An piece of assembly source code failed to parse.
    ParseError {
        /// The context that the parse error occurred within.
        context: Rc<ObjSrcContext>,
        /// The parse error.
        error: ParseError,
    },
    /// Encountered an error while trying to fetch data from a file.
    SrcCacheError {
        /// The joined path for the source file that couldn't be fetched.
        path: Rc<str>,
        /// The source code location for the expression that determined the
        /// file to be fetched.
        path_loc: ObjSrcLoc,
        /// The error from the source cache.
        error: SrcCacheError,
    },
    /// Encountered a static evaluation error in an expression that would
    /// inevitably cause linking to fail.
    StaticEvalError {
        /// The context that the expression appeared within.
        context: Rc<ObjSrcContext>,
        /// The evaluation error that would occur if the expression were to be
        /// evaluated.
        error: ExprEvalError,
    },
    /// Tried to reference an architecture that was never defined.
    UnknownArch {
        /// The name of the undefined architecture.
        arch: Rc<str>,
        /// The source code location for the expression that evaluated to the
        /// unknown architecture name.
        loc: ObjSrcLoc,
    },
    /// Tried to reference a charmap that was never defined.
    UnknownCharmap {
        /// The name of the undefined charmap.
        charmap: Rc<str>,
        /// The source code location for the expression that evaluated to the
        /// unknown charmap name.
        loc: ObjSrcLoc,
    },
    /// Tried to use an undeclared placeholder in a macro definition.
    UnknownMacroPlaceholder {
        /// The name of the undefined placeholder.
        name: Rc<str>,
        /// The source code location for the placeholder.
        loc: ObjSrcLoc,
    },
    /// Tried to use an undeclared struct name as a type specifier.
    UnknownStruct {
        /// The name of the undefined struct.
        name: Rc<str>,
        /// The source code location for the name.
        loc: ObjSrcLoc,
    },
    /// Tried to modify a variable that was never declared.
    UnknownVariable {
        /// The name of the undeclared variable.
        name: Rc<str>,
        /// The source code location for the unknown variable name.
        loc: ObjSrcLoc,
    },
    /// Found a macro invocation with no matching macro definition.
    UnmatchedMacroInvocation {
        /// The name of the macro.
        macro_name: Rc<str>,
        /// The name of the current architecture.
        arch: Rc<str>,
        /// The source code location for the macro invocation.
        invocation_loc: ObjSrcLoc,
    },
    /// Tried to assign an expression of one type to an lvalue of a different
    /// type.
    VariableTypeError {
        /// The source code location for the right-hand expression.
        expr_loc: ObjSrcLoc,
        /// The type of the expression.
        expr_type: ExprType,
        /// The source code location for the lvalue.
        lvalue_loc: ObjSrcLoc,
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
            Self::CharmapAlreadyDefined { name, name_loc, prev_loc } => {
                let message = format!("charmap {name:?} was already defined");
                let label1 = "previously defined here";
                let label2 = "defined again here";
                SourceError::new(name_loc.primary(), message)
                    .with_label(prev_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*name_loc.context)
            }
            Self::CharmapKeyConflict {
                key_loc,
                key_string,
                prev_loc,
                prev_string,
            } => {
                let message = "Conflicting mapping key in charmap";
                let label1 = format!("Previous key {prev_string:?}...");
                let label2 = format!("...conflicts with key {key_string:?}");
                SourceError::new(key_loc.primary(), message)
                    .with_label(prev_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*key_loc.context)
            }
            Self::CharmapMappingTypeError {
                from_loc,
                from_type,
                to_loc,
                to_type,
            } => {
                let message = format!(
                    "A charmap cannot map from {from_type} to {to_type}"
                );
                let label1 = format!("this has type {from_type}");
                let label2 = format!("this has type {to_type}");
                // TODO: add hint with acceptable types
                SourceError::new(from_loc.primary(), message)
                    .with_primary_label(label1)
                    .with_label(to_loc.primary(), label2)
                    .with_context(&*from_loc.context)
            }
            Self::CharmapRangeMappingEmptyByteRange {
                range_loc,
                range_start,
                range_last,
            } => {
                let message = "Empty byte range in charmap mapping";
                let label = format!(
                    "${range_start:02x} is greater than ${range_last:02x}"
                );
                SourceError::new(range_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*range_loc.context)
            }
            Self::CharmapRangeMappingEmptyCharRange {
                range_loc,
                range_start,
                range_last,
            } => {
                let message = "Empty character range in charmap mapping";
                let label =
                    format!("{range_start:?} is greater than {range_last:?}");
                SourceError::new(range_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*range_loc.context)
            }
            Self::CharmapRangeMappingInvalidCharEndpoint {
                range_loc,
                range_endpoint,
            } => {
                let message =
                    "Character range endpoints must be single characters";
                let label =
                    format!("{range_endpoint:?} isn't a single character");
                SourceError::new(range_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*range_loc.context)
            }
            Self::CharmapRangeMappingWithUnequalLengths {
                char_range_loc,
                char_range_len,
                byte_range_loc,
                byte_range_len,
            } => {
                let message = "Unequal range lengths in charmap mapping";
                let label1 = format!(
                    "This range spans {char_range_len} character{}",
                    if char_range_len == 1 { "" } else { "s" }
                );
                let label2 = format!(
                    "This range spans {byte_range_len} byte value{}",
                    if byte_range_len == 1 { "" } else { "s" }
                );
                SourceError::new(char_range_loc.primary(), message)
                    .with_primary_label(label1)
                    .with_label(byte_range_loc.primary(), label2)
                    .with_context(&*char_range_loc.context)
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
                let label = format!("this evaluates to {expr_value}");
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
                let label = format!("this has type {expr_type}");
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
                macro_name,
                placeholder_name,
                placeholder_loc,
                prev_loc,
            } => {
                let message = format!(
                    "Duplicate `{placeholder_name}` placeholder in macro \
                     `{macro_name}`"
                );
                let label1 = "Previously declared here";
                let label2 = "Duplicated here";
                SourceError::new(placeholder_loc.primary(), message)
                    .with_label(prev_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*placeholder_loc.context)
            }
            Self::DuplicateStructField {
                struct_name,
                field_name,
                field_loc,
                prev_loc,
            } => {
                let message = format!(
                    "Duplicate `{field_name}` field in struct `{struct_name}`"
                );
                let label1 = "Previously declared here";
                let label2 = "Duplicated here";
                SourceError::new(field_loc.primary(), message)
                    .with_label(prev_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*field_loc.context)
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
                let label = format!("this evaluates to ${expr_value:x}");
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*expr_loc.context)
            }
            Self::InvalidAsciiString { expr_loc, non_ascii_char } => {
                let message = "invalid ASCII string";
                let label =
                    format!("contains non-ASCII character '{non_ascii_char}'");
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*expr_loc.context)
            }
            Self::InvalidAsciiValue { expr_loc, expr_value } => {
                let message = "invalid ASCII value";
                let label = format!("this evaluates to {expr_value}");
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
                let label = format!("this evaluates to {expr_value}");
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
            Self::NameAlreadyDeclared { full_name, name_loc, prev_loc } => {
                let message = format!("`{full_name}` was already declared");
                let label1 = "previously declared here";
                let label2 = "redeclared here";
                SourceError::new(name_loc.primary(), message)
                    .with_label(prev_loc.primary(), label1)
                    .with_primary_label(label2)
                    .with_context(&*name_loc.context)
            }
            Self::NegativeRepeatCount { expr_loc, expr_value } => {
                let message = "negative repeat count";
                let label = format!("this evaluates to {expr_value}");
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*expr_loc.context)
            }
            Self::NoCharmapSet { expr_loc } => {
                let message = "cannot translate string without a charmap set";
                // TODO: add a hint for setting a charmap, or using .UTF8 or
                // .ASCII instead
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*expr_loc.context)
            }
            Self::NoMatchingCharmapMapping {
                charmap,
                expr_loc,
                unmatched,
            } => {
                let message = format!(
                    "no mapping for {unmatched:?} in charset {charmap:?}"
                );
                let label = format!("found unmapped substring {unmatched:?}");
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*expr_loc.context)
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
            Self::StaticEvalError { context, error } => {
                error.to_source_error(&context.path).with_context(&*context)
            }
            Self::UnknownArch { arch, loc } => {
                let message =
                    format!("the `{arch}` architecture was never defined");
                let label = format!("this evaluates to {arch:?}");
                SourceError::new(loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*loc.context)
            }
            Self::UnknownCharmap { charmap, loc } => {
                let message =
                    format!("no `{charmap}` charmap was ever defined");
                let label = format!("this evaluates to {charmap:?}");
                SourceError::new(loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*loc.context)
            }
            Self::UnknownMacroPlaceholder { name, loc } => {
                let message = format!("undeclared placeholder: `{name}`");
                let label = "not part of the macro signature";
                SourceError::new(loc.primary(), message)
                    .with_primary_label(label)
                    .with_context(&*loc.context)
            }
            Self::UnknownStruct { name, loc } => {
                let message = format!("no such struct: `{name}`");
                let label = "this was never declared";
                SourceError::new(loc.primary(), message)
                    .with_primary_label(label)
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
                let label1 = format!("this has type {expr_type}");
                let label2 = format!("this has type {lvalue_type}");
                SourceError::new(expr_loc.primary(), message)
                    .with_primary_label(label1)
                    .with_label(lvalue_loc.primary(), label2)
                    .with_context(&*lvalue_loc.context)
            }
        }
    }
}

//===========================================================================//
