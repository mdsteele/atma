use atma::db::{AdsEnvironment, AdsError};
use atma::error::StrSrcCache;
use atma::expr::{
    ExprNotStaticReason, ExprType, ExprTypeError, TemplateParseError,
};
use atma::parse::UnOpAst;
use atma::system::SimSystem;
use std::assert_matches;
use std::rc::Rc;

//===========================================================================//

fn expr_errors(source: &str) -> Vec<ExprTypeError> {
    let mut cache = StrSrcCache::new();
    let path = Rc::<str>::from("input");
    let ads = format!("print {source}\n");
    let bus = atma::bus::new_open_bus(0);
    let cpu = atma::proc::NopProc::new();
    let sim = SimSystem::new(vec![(Rc::from("cpu"), (Box::new(cpu), bus))]);
    let output = Vec::<u8>::new();
    match AdsEnvironment::create(&mut cache, path, &ads, sim, output) {
        Ok(_) => vec![],
        Err(errs) => errs
            .into_iter()
            .map(|error| {
                if let AdsError::ExprTypeError { error, .. } = error {
                    error
                } else {
                    panic!("unexpected error: {error:?}")
                }
            })
            .collect::<Vec<ExprTypeError>>(),
    }
}

//===========================================================================//

// TODO: more tests

#[test]
fn cannot_apply_unary_op_to_type() {
    let source = "!0";
    assert_matches!(
        expr_errors(source).as_slice(),
        [ExprTypeError::CannotApplyUnaryOpToType {
            op_span: _,
            op: UnOpAst::LogNot,
            arg_span: _,
            arg_type: ExprType::Integer,
        }]
    );
}

#[test]
fn cannot_interpolate_type_into_template() {
    let source = "\"{}-{:x}\" %% (1, %false)";
    assert_matches!(expr_errors(source).as_slice(), [
        ExprTypeError::CannotInterpolateTypeIntoTemplate {
            op_span: _,
            template_span: _,
            arg_span: _,
            arg_type: ExprType::Tuple(arg_items),
            param_type: ExprType::Tuple(param_items),
        },
    ] if &**arg_items == &[ExprType::Integer, ExprType::Boolean]
      && &**param_items == &[ExprType::Undefined, ExprType::Integer]);
}

#[test]
fn cannot_use_type_as_template() {
    let source = "1234 %% 5";
    assert_matches!(
        expr_errors(source).as_slice(),
        [ExprTypeError::CannotUseTypeAsTemplate {
            template_span: _,
            template_type: ExprType::Integer,
        }]
    );
}

#[test]
fn interpolation_template_skipped_arg_index() {
    let source = "(\"{}-\" ++ \"{3}-{}\") %% (0, 1, 2, 3)";
    assert_matches!(expr_errors(source).as_slice(), [
        ExprTypeError::InterpolationTemplateParseError {
            template_span: _,
            template_string,
            error: TemplateParseError::SkippedArgIndex(2),
        },
    ] if &**template_string == "{}-{3}-{}");
}

#[test]
fn tuple_index_not_static() {
    let source = "(1, 2, 3)[PC]";
    assert_matches!(expr_errors(source).as_slice(), [
        ExprTypeError::TupleIndexNotStatic {
            index_span: _,
            reason: ExprNotStaticReason::Variable { span: _, name },
        },
    ] if &**name == "PC");
}

#[test]
fn unknown_identifier() {
    let source = "42 + foo";
    assert_matches!(expr_errors(source).as_slice(), [
        ExprTypeError::UnknownIdentifier { span: _, name },
    ] if &**name == "foo");
}

//===========================================================================//
