use crate::expr::{ExprFunc, ExprType, ExprValue};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(crate) fn make_global_builtin_values()
-> HashMap<Rc<str>, (ExprValue, ExprType)> {
    let mut builtins = HashMap::<Rc<str>, (ExprValue, ExprType)>::new();

    // Add [int -> int] functions:
    let signature = Rc::new((ExprType::Integer, ExprType::Integer));
    for func in [ExprFunc::Cbrtz, ExprFunc::Sqrtz] {
        add_builtin_function(func, signature.clone(), &mut builtins);
    }

    // Add [(int, int) -> int] functions:
    let signature = Rc::new((
        ExprType::Tuple(Rc::from([ExprType::Integer, ExprType::Integer])),
        ExprType::Integer,
    ));
    for func in [
        ExprFunc::Divc,
        ExprFunc::Divf,
        ExprFunc::Divu,
        ExprFunc::Divx,
        ExprFunc::Divz,
    ] {
        add_builtin_function(func, signature.clone(), &mut builtins);
    }

    // Add [str -> !] functions:
    let signature = Rc::new((ExprType::String, ExprType::Bottom));
    add_builtin_function(ExprFunc::Error, signature, &mut builtins);

    builtins
}

fn add_builtin_function(
    func: ExprFunc,
    signature: Rc<(ExprType, ExprType)>,
    builtins: &mut HashMap<Rc<str>, (ExprValue, ExprType)>,
) {
    let value = ExprValue::Function(func);
    let expr_type = ExprType::Function(signature);
    builtins.insert(Rc::from(func.name()), (value, expr_type));
}

//===========================================================================//
