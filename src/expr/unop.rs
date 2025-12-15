use crate::expr::{ExprType, ExprValue};
use crate::parse::{ParseError, ParseResult, SrcSpan, UnOpAst};

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExprUnOp {
    BoolNot,
    IntBitNot,
    IntNeg,
}

impl ExprUnOp {
    pub(crate) fn typecheck(
        (op_span, op): (SrcSpan, UnOpAst),
        sub_span: SrcSpan,
        sub_type: ExprType,
    ) -> ParseResult<(ExprUnOp, ExprType)> {
        match (op, sub_type) {
            (UnOpAst::Neg, ExprType::Integer) => {
                Ok((ExprUnOp::IntNeg, ExprType::Integer))
            }
            (UnOpAst::LogNot, ExprType::Boolean) => {
                Ok((ExprUnOp::BoolNot, ExprType::Boolean))
            }
            (UnOpAst::BitNot, ExprType::Integer) => {
                Ok((ExprUnOp::IntBitNot, ExprType::Integer))
            }
            (op, sub_type) => {
                let verb = op.verb();
                let message = format!("Cannot {verb} {sub_type}");
                let label = format!("this expression has type {sub_type}");
                Err(vec![
                    ParseError::new(op_span, message)
                        .with_label(sub_span, label),
                ])
            }
        }
    }

    pub(crate) fn evaluate(self, sub: ExprValue) -> ExprValue {
        match self {
            ExprUnOp::BoolNot => ExprValue::Boolean(!sub.unwrap_bool()),
            ExprUnOp::IntBitNot => ExprValue::Integer(!sub.unwrap_int()),
            ExprUnOp::IntNeg => ExprValue::Integer(-sub.unwrap_int()),
        }
    }
}

//===========================================================================//
