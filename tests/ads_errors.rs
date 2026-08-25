use atma::db::{AdsEnvironment, AdsError};
use atma::error::StrSrcCache;
use atma::expr::{ExprNotStaticReason, ExprType, ExprTypeError};
use atma::system::SimSystem;
use std::assert_matches;
use std::rc::Rc;

//===========================================================================//

fn ads_errors(source: &str) -> Vec<AdsError> {
    let mut cache = StrSrcCache::new();
    let path = Rc::<str>::from("input");
    let bus = atma::bus::new_ram_bus(Box::new([0u8; 0x100]));
    let cpu = atma::proc::NopProc::new();
    let sim = SimSystem::new(vec![(Rc::from("cpu"), (Box::new(cpu), bus))]);
    let output = Vec::<u8>::new();
    match AdsEnvironment::create(&mut cache, path, source, sim, output) {
        Ok(_) => vec![],
        Err(errs) => errs.into_iter().collect::<Vec<_>>(),
    }
}

//===========================================================================//

#[test]
fn cannot_modify_constant() {
    let source = r#"\
    let foo = %sqrtz(1)
    set foo = 2
    "#;
    assert_matches!(ads_errors(source).as_slice(), [
        AdsError::CannotModifyConstant { name, .. },
    ] if &**name == "foo");
}

#[test]
fn expr_type_error() {
    let source = r#"\
    let foo = {1, 2, 3}#[0]
    "#;
    assert_matches!(
        ads_errors(source).as_slice(),
        [AdsError::ExprTypeError {
            error: ExprTypeError::CannotIndexIntoType {
                indexed_type: ExprType::Integer,
                ..
            },
            ..
        }]
    );
}

#[test]
fn path_not_static() {
    let source = r#"\
    var foobar_path = "foo/bar.ads"
    use foobar_path
    "#;
    assert_matches!(ads_errors(source).as_slice(), [
        AdsError::PathNotStatic {
            reason: ExprNotStaticReason::Variable { name, .. },
            ..
        },
    ] if &**name == "foobar_path");
}

#[test]
fn path_type_error() {
    let source = r#"\
    use 12345
    "#;
    assert_matches!(
        ads_errors(source).as_slice(),
        [AdsError::PathTypeError { expr_type: ExprType::Integer, .. }]
    );
}

#[test]
fn proc_not_static() {
    let source = r#"\
    var foo = "bar"
    with foo
    "#;
    assert_matches!(ads_errors(source).as_slice(), [
        AdsError::ProcNotStatic {
            reason: ExprNotStaticReason::Variable { name, .. },
            ..
        },
    ] if &**name == "foo");
}

#[test]
fn proc_type_error() {
    let source = r#"\
    with 12345
    "#;
    assert_matches!(
        ads_errors(source).as_slice(),
        [AdsError::ProcTypeError { expr_type: ExprType::Integer, .. }]
    );
}

#[test]
fn unknown_proc() {
    let source = r#"\
    with "no-such-proc"
    "#;
    assert_matches!(ads_errors(source).as_slice(), [
        AdsError::UnknownProc { proc_name, .. },
    ] if &**proc_name == "no-such-proc");
}

#[test]
fn unknown_variable() {
    let source = r#"\
    set foo = 1
    "#;
    assert_matches!(ads_errors(source).as_slice(), [
        AdsError::UnknownVariable { name, .. },
    ] if &**name == "foo");
}

//===========================================================================//
