use atma;
use atma::addr::AlignTryFromError;
use atma::asm::AsmError;
use atma::expr::ExprType;
use num_bigint::BigInt;
use std::assert_matches;
use std::rc::Rc;

//===========================================================================//

fn asm_errors(source: &str) -> Vec<AsmError> {
    let asm_path = Rc::<str>::from("input");
    let asm_source = Rc::<str>::from(source);
    let mut cache = atma::error::StrSrcCache::new();
    cache.add_source(asm_path.clone(), asm_source.clone());
    match atma::asm::assemble_source(&mut cache, asm_path, &asm_source) {
        Ok(_) => vec![],
        Err(errs) => errs.into_iter().collect::<Vec<_>>(),
    }
}

//===========================================================================//

#[test]
fn arch_has_no_endianness() {
    let source = r#"\
    .SECTION "TEST", arch="none"
        .u16 1234
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::ArchHasNoEndianness{ directive: ".U16", arch, .. },
    ] if &**arch == "none");
}

#[test]
fn assertion_statically_failed() {
    let source = r#"\
    .ASSERT %false, "oops"
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::AssertionStaticallyFailed{ additional_message: Some(m), .. },
    ] if &**m == "oops");
}

#[test]
fn decl_name_is_builtin() {
    let source = r#"\
    .IMPORT %sqrtz
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::DeclNameIsBuiltin{ name, .. },
    ] if &**name == "%sqrtz");
}

#[test]
fn directive_expr_out_of_range() {
    let source = r#"\
    .SECTION "TEST"
        .u8 256
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::DirectiveExprOutOfRange{
            directive: ".U8",
            component: "value",
            expr_value,
            ..
        },
    ] if *expr_value == BigInt::from(256));
}

#[test]
fn directive_expr_type_error() {
    let source = r#"\
    .SECTION "TEST", within="page"
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::DirectiveExprTypeError{
            directive: ".SECTION",
            component: "within",
            expr_type: ExprType::String,
            valid_types,
            ..
        },
    ] if valid_types == &[ExprType::Integer]);
}

#[test]
fn directive_not_in_section() {
    let source = r#"\
    .U16LE $1234
    "#;
    assert_matches!(
        asm_errors(source).as_slice(),
        [AsmError::DirectiveNotInSection { directive: ".U16LE", .. }]
    );
}

#[test]
fn duplicate_attr_name() {
    let source = r#"\
    .SECTION "TEST", align=$10, align=$10
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::DuplicateAttrName{ directive: ".SECTION", attr_name, .. },
    ] if &**attr_name == "align");
}

#[test]
fn duplicate_macro_placeholder() {
    let source = r#"\
    .DEFMACRO FOO %BAR, %BAR {
        .u8 %BAR
        .u8 %BAR
    }
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::DuplicateMacroPlaceholder{ placeholder_name, .. },
    ] if &**placeholder_name == "%BAR");
}

#[test]
fn invalid_alignment_value() {
    let source = r#"\
    .SECTION "TEST", align=$18
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::InvalidAlignmentValue{
            directive: ".SECTION",
            attr_name,
            error: AlignTryFromError::NotAPowerOfTwo,
            expr_value,
            ..
        },
    ] if &**attr_name == "align" && *expr_value == BigInt::from(0x18));
}

#[test]
fn invalid_attr_name() {
    let source = r#"\
    .SECTION "TEST", blarg=0
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::InvalidAttrName{
            directive: ".SECTION",
            attr_name,
            ..
        },
    ] if &**attr_name == "blarg");
}

#[test]
fn invalid_unicode_scalar_value() {
    let source = r#"\
    .SECTION "TEST"
        .utf8 $df00
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::InvalidUnicodeScalarValue{ expr_value, .. },
    ] if *expr_value == BigInt::from(0xdf00));
}

#[test]
fn multiple_macro_placeholders() {
    let source = r#"\
    .DEFMACRO FOO [%BAR + %BAZ] {
        .u8 %BAR
        .u8 %BAZ
    }
    "#;
    assert_matches!(
        asm_errors(source).as_slice(),
        [AsmError::MultipleMacroPlaceholders { .. }]
    );
}

#[test]
fn symbol_already_declared() {
    let source = r#"\
    .SECTION "TEST"
    foo: .u8 1
    foo: .u8 2
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::SymbolAlreadyDeclared{ full_name, .. },
    ] if &**full_name == "foo");
}

#[test]
fn unknown_arch() {
    let source = r#"\
    .SECTION "TEST", arch="x86"
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::UnknownArch{ arch, .. },
    ] if &**arch == "x86");
}

#[test]
fn unknown_macro_placeholder() {
    let source = r#"\
    .DEFMACRO FOO %BAR {
        .u8 %BAZ
    }
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::UnknownMacroPlaceholder{ name, .. },
    ] if &**name == "%BAZ");
}

#[test]
fn unmatched_macro_invocation() {
    let source = r#"\
    .SECTION "TEST", arch="6502"
        XCE  ; 65C816-only instruction
    .END
    "#;
    assert_matches!(asm_errors(source).as_slice(), [
        AsmError::UnmatchedMacroInvocation{ macro_name, arch, .. },
    ] if &**macro_name == "XCE" && &**arch == "6502");
}

//===========================================================================//
