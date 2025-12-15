use crate::expr::{ExprType, ExprValue};
use crate::parse::{BinOpAst, ParseError, ParseResult, SrcSpan};
use num_bigint::{BigInt, BigUint};
use num_traits::{Euclid, Pow, Signed, ToPrimitive, Zero};

//===========================================================================//

/// An error that can occur while evaluating an [ExprBinOp] on two
/// [ExprValue]s.
#[derive(Debug, Eq, PartialEq)]
pub(crate) enum ExprBinOpEvalError {
    /// Tried to bit shift an integer left/right by the given number of bits,
    /// but the shift amount was negative.
    BitShiftByNegative(BigInt),
    /// Tried to bit shift an integer left/right by the given number of bits,
    /// but the shift amount was too large.
    BitShiftOutOfRange(BigUint),
    /// Tried to divide an integer, but the divisor was zero.
    DivideByZero,
    /// Tried to modulo an integer, but the modulus was zero.
    ModByZero,
    /// Tried to exponentiate an integer with the given exponent, but the
    /// exponent was negative.
    PowNegativeExponent(BigInt),
}

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExprBinOp {
    AnyCmpEq,
    AnyCmpLe,
    AnyCmpLt,
    AnyCmpGe,
    AnyCmpGt,
    AnyCmpNe,
    IntAdd,
    IntBitAnd,
    IntBitOr,
    IntBitXor,
    IntDiv,
    IntMod,
    IntMul,
    IntPow,
    IntShl,
    IntShr,
    IntSub,
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
            (BinOpAst::Add, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntAdd, ExprType::Integer))
            }
            (BinOpAst::BitAnd, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntBitAnd, ExprType::Integer))
            }
            (BinOpAst::BitOr, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntBitOr, ExprType::Integer))
            }
            (BinOpAst::BitXor, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntBitXor, ExprType::Integer))
            }
            (BinOpAst::CmpEq, t1, t2) if t1 == t2 => {
                Ok((ExprBinOp::AnyCmpEq, ExprType::Boolean))
            }
            (BinOpAst::CmpLe, t1, t2) if t1 == t2 && t1.is_ord() => {
                Ok((ExprBinOp::AnyCmpLe, ExprType::Boolean))
            }
            (BinOpAst::CmpLt, t1, t2) if t1 == t2 && t1.is_ord() => {
                Ok((ExprBinOp::AnyCmpLt, ExprType::Boolean))
            }
            (BinOpAst::CmpGe, t1, t2) if t1 == t2 && t1.is_ord() => {
                Ok((ExprBinOp::AnyCmpGe, ExprType::Boolean))
            }
            (BinOpAst::CmpGt, t1, t2) if t1 == t2 && t1.is_ord() => {
                Ok((ExprBinOp::AnyCmpGt, ExprType::Boolean))
            }
            (BinOpAst::CmpNe, t1, t2) if t1 == t2 => {
                Ok((ExprBinOp::AnyCmpNe, ExprType::Boolean))
            }
            (BinOpAst::Div, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntDiv, ExprType::Integer))
            }
            (BinOpAst::Mod, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntMod, ExprType::Integer))
            }
            (BinOpAst::Mul, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntMul, ExprType::Integer))
            }
            (BinOpAst::Pow, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntPow, ExprType::Integer))
            }
            (BinOpAst::Shl, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntShl, ExprType::Integer))
            }
            (BinOpAst::Shr, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntShr, ExprType::Integer))
            }
            (BinOpAst::Sub, ExprType::Integer, ExprType::Integer) => {
                Ok((ExprBinOp::IntSub, ExprType::Integer))
            }
            (op, lhs_type, rhs_type) => {
                let (verb, conj, rev) = op.verb_conj_rev();
                let message = if rev {
                    format!("Cannot {verb} {rhs_type} {conj} {lhs_type}")
                } else {
                    format!("Cannot {verb} {lhs_type} {conj} {rhs_type}")
                };
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

    pub(crate) fn evaluate(
        self,
        lhs: ExprValue,
        rhs: ExprValue,
    ) -> Result<ExprValue, ExprBinOpEvalError> {
        match self {
            ExprBinOp::AnyCmpEq => Ok(ExprValue::Boolean(lhs == rhs)),
            ExprBinOp::AnyCmpLe => Ok(ExprValue::Boolean(lhs <= rhs)),
            ExprBinOp::AnyCmpLt => Ok(ExprValue::Boolean(lhs < rhs)),
            ExprBinOp::AnyCmpGe => Ok(ExprValue::Boolean(lhs >= rhs)),
            ExprBinOp::AnyCmpGt => Ok(ExprValue::Boolean(lhs > rhs)),
            ExprBinOp::AnyCmpNe => Ok(ExprValue::Boolean(lhs != rhs)),
            ExprBinOp::IntAdd => {
                Ok(ExprValue::Integer(lhs.unwrap_int() + rhs.unwrap_int()))
            }
            ExprBinOp::IntBitAnd => {
                Ok(ExprValue::Integer(lhs.unwrap_int() & rhs.unwrap_int()))
            }
            ExprBinOp::IntBitOr => {
                Ok(ExprValue::Integer(lhs.unwrap_int() | rhs.unwrap_int()))
            }
            ExprBinOp::IntBitXor => {
                Ok(ExprValue::Integer(lhs.unwrap_int() ^ rhs.unwrap_int()))
            }
            ExprBinOp::IntDiv => {
                let divisor = rhs.unwrap_int();
                if divisor.is_zero() {
                    Err(ExprBinOpEvalError::DivideByZero)
                } else {
                    Ok(ExprValue::Integer(
                        lhs.unwrap_int().div_euclid(&divisor),
                    ))
                }
            }
            ExprBinOp::IntMod => {
                let modulus = rhs.unwrap_int();
                if modulus.is_zero() {
                    Err(ExprBinOpEvalError::ModByZero)
                } else {
                    Ok(ExprValue::Integer(
                        lhs.unwrap_int().rem_euclid(&modulus),
                    ))
                }
            }
            ExprBinOp::IntMul => {
                Ok(ExprValue::Integer(lhs.unwrap_int() * rhs.unwrap_int()))
            }
            ExprBinOp::IntPow => {
                let exponent = rhs.unwrap_int();
                if exponent.is_negative() {
                    Err(ExprBinOpEvalError::PowNegativeExponent(exponent))
                } else {
                    Ok(ExprValue::Integer(
                        lhs.unwrap_int().pow(exponent.magnitude()),
                    ))
                }
            }
            ExprBinOp::IntShl => {
                let shift = ExprBinOp::get_bit_shift_amount(rhs.unwrap_int())?;
                Ok(ExprValue::Integer(lhs.unwrap_int() << shift))
            }
            ExprBinOp::IntShr => {
                let shift = ExprBinOp::get_bit_shift_amount(rhs.unwrap_int())?;
                Ok(ExprValue::Integer(lhs.unwrap_int() >> shift))
            }
            ExprBinOp::IntSub => {
                Ok(ExprValue::Integer(lhs.unwrap_int() - rhs.unwrap_int()))
            }
        }
    }

    fn get_bit_shift_amount(
        signed_shift: BigInt,
    ) -> Result<u16, ExprBinOpEvalError> {
        if signed_shift.is_negative() {
            Err(ExprBinOpEvalError::BitShiftByNegative(signed_shift))
        } else {
            let unsigned_shift = signed_shift.into_parts().1;
            if let Some(shift) = unsigned_shift.to_u16() {
                Ok(shift)
            } else {
                Err(ExprBinOpEvalError::BitShiftOutOfRange(unsigned_shift))
            }
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ExprBinOp, ExprBinOpEvalError};
    use crate::expr::ExprValue;
    use num_bigint::BigInt;

    fn int_value(value: i32) -> ExprValue {
        ExprValue::Integer(BigInt::from(value))
    }

    #[test]
    fn divide_by_zero() {
        assert_eq!(
            ExprBinOp::IntDiv.evaluate(int_value(1), int_value(0)),
            Err(ExprBinOpEvalError::DivideByZero)
        );
    }

    #[test]
    fn modulo_by_zero() {
        assert_eq!(
            ExprBinOp::IntMod.evaluate(int_value(1), int_value(0)),
            Err(ExprBinOpEvalError::ModByZero)
        );
    }

    #[test]
    fn pow_by_negative() {
        assert_eq!(
            ExprBinOp::IntPow.evaluate(int_value(20), int_value(-5)),
            Err(ExprBinOpEvalError::PowNegativeExponent(BigInt::from(-5)))
        );
    }

    #[test]
    fn shift_by_negative() {
        assert_eq!(
            ExprBinOp::IntShl.evaluate(int_value(16), int_value(-2)),
            Err(ExprBinOpEvalError::BitShiftByNegative(BigInt::from(-2)))
        );
        assert_eq!(
            ExprBinOp::IntShr.evaluate(int_value(16), int_value(-2)),
            Err(ExprBinOpEvalError::BitShiftByNegative(BigInt::from(-2)))
        );
    }
}

//===========================================================================//
