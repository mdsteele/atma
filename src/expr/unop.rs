use super::error::{ExprTypeError, ExprTypeResult};
use crate::error::SrcSpan;
use crate::expr::{ExprType, ExprValue};
use crate::parse::UnOpAst;

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExprUnOp {
    BoolNot,
    IntBitNot,
    IntNeg,
}

impl ExprUnOp {
    pub(super) fn typecheck(
        (op_span, op): (SrcSpan, UnOpAst),
        arg_span: SrcSpan,
        arg_type: ExprType,
    ) -> ExprTypeResult<(ExprUnOp, ExprType)> {
        match (op, arg_type) {
            (UnOpAst::Neg, ExprType::Integer) => {
                Ok((ExprUnOp::IntNeg, ExprType::Integer))
            }
            (UnOpAst::LogNot, ExprType::Boolean) => {
                Ok((ExprUnOp::BoolNot, ExprType::Boolean))
            }
            (UnOpAst::BitNot, ExprType::Integer) => {
                Ok((ExprUnOp::IntBitNot, ExprType::Integer))
            }
            (op, arg_type) => {
                Err(vec![ExprTypeError::CannotApplyUnaryOpToType {
                    op_span,
                    op,
                    arg_span,
                    arg_type,
                }])
            }
        }
    }

    pub(crate) fn evaluate(self, arg: ExprValue) -> ExprValue {
        match self {
            ExprUnOp::BoolNot => ExprValue::Boolean(!arg.unwrap_bool()),
            ExprUnOp::IntBitNot => ExprValue::Integer(!arg.unwrap_int()),
            ExprUnOp::IntNeg => ExprValue::Integer(-arg.unwrap_int()),
        }
    }
}

//===========================================================================//
