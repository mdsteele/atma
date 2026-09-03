use super::error::AsmError;
use crate::error::Errs;
use crate::expr::{ExprStatic, ExprType, ExprTypeError, ExprValue};
use crate::obj::ObjSrcLoc;
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

type ValueIterator = Box<dyn Iterator<Item = ExprValue>>;

//===========================================================================//

pub(super) fn typecheck_iterator(
    expr_loc: ObjSrcLoc,
    expr_type: ExprType,
    expr_static: ExprStatic,
) -> ((ExprType, ValueIterator), Errs<AsmError>) {
    let mut errs = Errs::<AsmError>::new();
    let (item_type, iterator) = match (expr_type, expr_static) {
        (ExprType::Undefined, _) => (ExprType::Undefined, empty_iterator()),
        (ExprType::Integer, Ok(value)) => {
            let count = value.unwrap_int();
            let iterator = if count < BigInt::ZERO {
                errs.push(AsmError::NegativeRepeatCount {
                    expr_loc,
                    expr_value: count,
                });
                empty_iterator()
            } else {
                BigIntRange::boxed(BigInt::ZERO, count)
            };
            (ExprType::Integer, iterator)
        }
        (ExprType::List(item_type), Ok(value)) => {
            let iterator = ExprListIterator::boxed(value.unwrap_list());
            (Rc::unwrap_or_clone(item_type), iterator)
        }
        (
            ExprType::Bottom | ExprType::Integer | ExprType::List(_),
            Err(reason),
        ) => {
            errs.push(AsmError::DirectiveExprNotStatic {
                directive: ".REPEAT",
                component: "iterator",
                expr_loc: expr_loc.clone(),
                reason,
            });
            (ExprType::Undefined, empty_iterator())
        }
        (expr_type, _) => {
            errs.push(AsmError::ExprTypeError {
                context: expr_loc.context.clone(),
                error: ExprTypeError::CannotUseTypeAsIterator {
                    expr_span: expr_loc.span,
                    expr_type,
                },
            });
            (ExprType::Undefined, empty_iterator())
        }
    };
    ((item_type, iterator), errs)
}

//===========================================================================//

fn empty_iterator() -> ValueIterator {
    Box::new([].into_iter())
}

//===========================================================================//

struct BigIntRange {
    current: BigInt,
    end: BigInt,
}

impl BigIntRange {
    pub fn boxed(start: BigInt, end: BigInt) -> ValueIterator {
        Box::new(Self { current: start, end })
    }
}

impl Iterator for BigIntRange {
    type Item = ExprValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.end {
            None
        } else {
            let next = &self.current + &BigInt::ONE;
            let value = std::mem::replace(&mut self.current, next);
            Some(ExprValue::Integer(value))
        }
    }
}

//===========================================================================//

struct ExprListIterator {
    items: Rc<[ExprValue]>,
    index: usize,
}

impl ExprListIterator {
    pub fn boxed(items: Rc<[ExprValue]>) -> ValueIterator {
        Box::new(Self { items, index: 0 })
    }
}

impl Iterator for ExprListIterator {
    type Item = ExprValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.items.len() {
            None
        } else {
            let value = self.items[self.index].clone();
            self.index += 1;
            Some(value)
        }
    }
}

//===========================================================================//
