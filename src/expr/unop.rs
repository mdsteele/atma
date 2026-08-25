use super::error::{ExprEvalError, ExprTypeError, ExprTypeResult};
use crate::error::{Errs, SrcSpan};
use crate::expr::{ExprLabel, ExprType, ExprValue};
use crate::obj::{BinaryIo, Decoder, Encoder};
use crate::parse::UnOpAst;
use num_bigint::BigInt;
use std::io;

//===========================================================================//

const TAG_ADDR_OF: u8 = 0x00;
const TAG_BIT_NOT: u8 = 0x01;
const TAG_LENGTH: u8 = 0x02;
const TAG_NEG: u8 = 0x03;

//===========================================================================//

/// An error that can occur while evaluating an [ExprUnOp] on an [ExprValue].
#[derive(Debug, Eq, PartialEq)]
pub(crate) enum ExprUnOpEvalError {
    /// Tried to get the address of a label, but the label has not yet been
    /// resolved and its address is not yet known.
    AddrOfLabelUnresolved,
    /// Tried to perform a unary operation, but the operand had the wrong type
    /// at runtime.
    ///
    /// This should normally be prevented by static typechecking, but can occur
    /// due to e.g. a corrupted object file.
    InvalidType,
}

impl ExprUnOpEvalError {
    /// Converts `self` into an [`ExprEvalError`].
    pub(crate) fn into_expr_eval_error(
        self,
        op_span: SrcSpan,
        arg_span: SrcSpan,
    ) -> ExprEvalError {
        match self {
            Self::AddrOfLabelUnresolved => {
                ExprEvalError::AddrOfLabelUnresolved { op_span, arg_span }
            }
            Self::InvalidType => ExprEvalError::InvalidType { span: arg_span },
        }
    }
}

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExprUnOp {
    AddrOf,
    BitNot,
    Length,
    Neg,
}

impl ExprUnOp {
    pub(super) fn typecheck(
        (op_span, op): (SrcSpan, UnOpAst),
        arg_span: SrcSpan,
        arg_type: ExprType,
    ) -> ExprTypeResult<(Self, ExprType)> {
        match (op, arg_type) {
            (UnOpAst::AddrOf, ExprType::Label | ExprType::Bottom) => {
                Ok((Self::AddrOf, ExprType::Integer))
            }
            (UnOpAst::BitNot, ExprType::Integer | ExprType::Bottom) => {
                Ok((Self::BitNot, ExprType::Integer))
            }
            (UnOpAst::Length, ExprType::List(_) | ExprType::Bottom) => {
                Ok((Self::Length, ExprType::Integer))
            }
            (UnOpAst::LogNot, ExprType::Boolean | ExprType::Bottom) => {
                Ok((Self::BitNot, ExprType::Boolean))
            }
            (UnOpAst::Neg, ExprType::Integer | ExprType::Bottom) => {
                Ok((Self::Neg, ExprType::Integer))
            }
            (op, arg_type) => {
                Err(Errs::one(ExprTypeError::CannotApplyUnaryOpToType {
                    op_span,
                    op,
                    arg_span,
                    arg_type,
                }))
            }
        }
    }

    pub(crate) fn evaluate(
        self,
        arg: ExprValue,
    ) -> Result<ExprValue, ExprUnOpEvalError> {
        match self {
            Self::AddrOf => match arg {
                ExprValue::Label(ExprLabel::AddrAbsolute {
                    address, ..
                }) => Ok(ExprValue::Integer(address)),
                ExprValue::Label(_) => {
                    Err(ExprUnOpEvalError::AddrOfLabelUnresolved)
                }
                _ => Err(ExprUnOpEvalError::InvalidType),
            },
            Self::BitNot => match arg {
                ExprValue::Boolean(arg) => Ok(ExprValue::Boolean(!arg)),
                ExprValue::Integer(arg) => Ok(ExprValue::Integer(!arg)),
                _ => Err(ExprUnOpEvalError::InvalidType),
            },
            Self::Length => match arg {
                ExprValue::List(arg) => {
                    Ok(ExprValue::Integer(BigInt::from(arg.len())))
                }
                _ => Err(ExprUnOpEvalError::InvalidType),
            },
            Self::Neg => match arg {
                ExprValue::Integer(arg) => Ok(ExprValue::Integer(-arg)),
                _ => Err(ExprUnOpEvalError::InvalidType),
            },
        }
    }
}

impl BinaryIo for ExprUnOp {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_ADDR_OF => Ok(Self::AddrOf),
            TAG_BIT_NOT => Ok(Self::BitNot),
            TAG_LENGTH => Ok(Self::Length),
            TAG_NEG => Ok(Self::Neg),
            byte => Err(io::Error::other(format!(
                "unknown unop tag: 0x{:02x}",
                byte
            ))),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        let tag = match self {
            Self::AddrOf => TAG_ADDR_OF,
            Self::BitNot => TAG_BIT_NOT,
            Self::Length => TAG_LENGTH,
            Self::Neg => TAG_NEG,
        };
        tag.write_to(encoder)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::ExprUnOp;
    use crate::obj::assert_round_trips;

    #[test]
    fn binary_io_round_trip() {
        assert_round_trips(ExprUnOp::AddrOf);
        assert_round_trips(ExprUnOp::BitNot);
        assert_round_trips(ExprUnOp::Length);
        assert_round_trips(ExprUnOp::Neg);
    }
}

//===========================================================================//
