use crate::expr::{ExprType, ExprValue};
use crate::parse::{BinOpAst, ParseError, ParseResult, SrcSpan};

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExprBinOp {
    BoolCmpEq,
    IntCmpEq,
    IntPlus,
    StrCmpEq,
}

impl ExprBinOp {
    pub(crate) fn typecheck(
        (op_span, op): (SrcSpan, BinOpAst),
        lhs_span: SrcSpan,
        lhs_type: ExprType,
        rhs_span: SrcSpan,
        rhs_type: ExprType,
    ) -> ParseResult<(ExprBinOp, ExprType)> {
        match (op, lhs_type, rhs_type) {
            (BinOpAst::Plus, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntPlus, ExprType::Integer))
            }
            (BinOpAst::CmpEq, ExprType::Boolean, ExprType::Boolean) => {
                Ok((ExprBinOp::BoolCmpEq, ExprType::Boolean))
            }
            (BinOpAst::CmpEq, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntCmpEq, ExprType::Boolean))
            }
            (BinOpAst::CmpEq, ExprType::String, ExprType::String) => {
                Ok((ExprBinOp::StrCmpEq, ExprType::Boolean))
            }
            (op, lhs_type, rhs_type) => {
                let message =
                    format!("Cannot {} {lhs_type} and {rhs_type}", op.verb());
                let label1 = format!("this expression has type {lhs_type}");
                let label2 = format!("this expression has type {rhs_type}");
                Err(vec![
                    ParseError::new(op_span, message)
                        .with_label(lhs_span, label1)
                        .with_label(rhs_span, label2),
                ])
            }
        }
    }

    pub(crate) fn evaluate(self, lhs: ExprValue, rhs: ExprValue) -> ExprValue {
        match self {
            ExprBinOp::BoolCmpEq => {
                ExprValue::Boolean(lhs.unwrap_bool() == rhs.unwrap_bool())
            }
            ExprBinOp::IntCmpEq => {
                ExprValue::Boolean(lhs.unwrap_int() == rhs.unwrap_int())
            }
            ExprBinOp::IntPlus => {
                ExprValue::Integer(lhs.unwrap_int() + rhs.unwrap_int())
            }
            ExprBinOp::StrCmpEq => {
                ExprValue::Boolean(lhs.unwrap_str() == rhs.unwrap_str())
            }
        }
    }
}

//===========================================================================//
