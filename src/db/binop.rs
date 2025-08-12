use super::value::{AdsType, AdsValue};
use crate::parse::{BinOpAst, ParseError, SrcSpan};

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum AdsBinOp {
    BoolCmpEq,
    IntCmpEq,
    IntPlus,
}

impl AdsBinOp {
    pub(crate) fn typecheck(
        (op_span, op): (SrcSpan, BinOpAst),
        lhs_span: SrcSpan,
        lhs_type: AdsType,
        rhs_span: SrcSpan,
        rhs_type: AdsType,
    ) -> Result<(AdsBinOp, AdsType), Vec<ParseError>> {
        match (op, lhs_type, rhs_type) {
            (BinOpAst::Plus, AdsType::Integer, AdsType::Integer) => {
                Ok((AdsBinOp::IntPlus, AdsType::Integer))
            }
            (BinOpAst::CmpEq, AdsType::Boolean, AdsType::Boolean) => {
                Ok((AdsBinOp::BoolCmpEq, AdsType::Boolean))
            }
            (BinOpAst::CmpEq, AdsType::Integer, AdsType::Integer) => {
                Ok((AdsBinOp::IntCmpEq, AdsType::Boolean))
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

    pub(crate) fn evaluate(self, lhs: AdsValue, rhs: AdsValue) -> AdsValue {
        match self {
            AdsBinOp::BoolCmpEq => {
                AdsValue::Boolean(lhs.unwrap_bool() == rhs.unwrap_bool())
            }
            AdsBinOp::IntCmpEq => {
                AdsValue::Boolean(lhs.unwrap_int() == rhs.unwrap_int())
            }
            AdsBinOp::IntPlus => {
                AdsValue::Integer(lhs.unwrap_int() + rhs.unwrap_int())
            }
        }
    }
}

//===========================================================================//
