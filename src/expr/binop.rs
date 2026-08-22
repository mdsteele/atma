use super::error::{ExprEvalError, ExprTypeError, ExprTypeResult};
use crate::error::{Errs, SrcSpan};
use crate::expr::{ExprLabel, ExprType, ExprValue};
use crate::obj::{BinaryIo, Decoder, Encoder};
use crate::parse::BinOpAst;
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{Euclid, Pow, Signed, ToPrimitive, Zero};
use std::cmp::Ordering;
use std::io;
use std::rc::Rc;

//===========================================================================//

const TAG_ADD: u8 = 0x00;
const TAG_BIT_AND: u8 = 0x01;
const TAG_BIT_OR: u8 = 0x02;
const TAG_BIT_XOR: u8 = 0x03;
const TAG_BYTE: u8 = 0x04;
const TAG_CMP_EQ: u8 = 0x05;
const TAG_CMP_LE: u8 = 0x06;
const TAG_CMP_LT: u8 = 0x07;
const TAG_CMP_GE: u8 = 0x08;
const TAG_CMP_GT: u8 = 0x09;
const TAG_CMP_NE: u8 = 0x0a;
const TAG_CONCAT: u8 = 0x0b;
const TAG_DIV: u8 = 0x0c;
const TAG_MOD: u8 = 0x0d;
const TAG_MUL: u8 = 0x0e;
const TAG_POW: u8 = 0x0f;
const TAG_SHL: u8 = 0x10;
const TAG_SHR: u8 = 0x11;
const TAG_SUB: u8 = 0x12;

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
    /// Tried to select a byte at the given negative index.
    ByteSelectByNegative(BigInt),
    /// Tried to divide an integer, but the divisor was zero.
    DivideByZero,
    /// Tried to perform a binary operation, but one or both of the operands
    /// had the wrong type at runtime.
    ///
    /// This should normally be prevented by static typechecking, but can occur
    /// due to e.g. a corrupted object file.
    InvalidType,
    /// Tried to modulo an integer, but the modulus was zero.
    ModByZero,
    /// Tried to exponentiate an integer with the given exponent, but the
    /// exponent was negative.
    PowNegativeExponent(BigInt),
    /// Tried to subtract one label from another, but the labels were in the
    /// given two different address spaces.
    SubtractLabelsInDifferentAddrspaces(Rc<str>, Rc<str>),
    /// Tried to subtract one label from another, but the labels have not yet
    /// been resolved and the delta is not yet known.
    SubtractLabelsUnresolved,
}

impl ExprBinOpEvalError {
    /// Converts `self` into an [`ExprEvalError`].
    pub(crate) fn into_expr_eval_error(
        self,
        op_span: SrcSpan,
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) -> ExprEvalError {
        match self {
            Self::BitShiftByNegative(rhs_value) => {
                ExprEvalError::BitShiftByNegative { rhs_span, rhs_value }
            }
            Self::BitShiftOutOfRange(rhs_value) => {
                ExprEvalError::BitShiftOutOfRange { rhs_span, rhs_value }
            }
            Self::ByteSelectByNegative(rhs_value) => {
                ExprEvalError::ByteSelectByNegative { rhs_span, rhs_value }
            }
            Self::DivideByZero => ExprEvalError::DivideByZero { rhs_span },
            Self::InvalidType => ExprEvalError::InvalidType {
                span: lhs_span.merged_with(rhs_span),
            },
            Self::ModByZero => ExprEvalError::ModByZero { rhs_span },
            Self::PowNegativeExponent(rhs_value) => {
                ExprEvalError::PowNegativeExponent { rhs_span, rhs_value }
            }
            Self::SubtractLabelsInDifferentAddrspaces(
                lhs_space,
                rhs_space,
            ) => ExprEvalError::SubtractLabelsInDifferentAddrspaces {
                op_span,
                lhs_span,
                lhs_space,
                rhs_span,
                rhs_space,
            },
            Self::SubtractLabelsUnresolved => {
                ExprEvalError::SubtractLabelsUnresolved {
                    op_span,
                    lhs_span,
                    rhs_span,
                }
            }
        }
    }
}

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExprBinOp {
    Add,
    BitAnd,
    BitOr,
    BitXor,
    Byte,
    CmpEq,
    CmpLe,
    CmpLt,
    CmpGe,
    CmpGt,
    CmpNe,
    Concat,
    Div,
    Mod,
    Mul,
    Pow,
    Shl,
    Shr,
    Sub,
}

impl ExprBinOp {
    pub(super) fn typecheck(
        (op_span, op): (SrcSpan, BinOpAst),
        lhs_span: SrcSpan,
        lhs_type: ExprType,
        rhs_span: SrcSpan,
        rhs_type: ExprType,
    ) -> ExprTypeResult<(ExprBinOp, ExprType)> {
        // TODO: Allow one or both operands to have type `Bottom`, with result
        // type `Bottom`, as long as the other operand has a valid type.
        // (e.g. bottom & int is OK, but bottom & str is not OK.)
        match (op, lhs_type, rhs_type) {
            (BinOpAst::Add, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Add, ExprType::Integer))
            }
            (BinOpAst::Add, ExprType::Integer, ExprType::Label) => {
                Ok((Self::Add, ExprType::Label))
            }
            (BinOpAst::Add, ExprType::Label, ExprType::Integer) => {
                Ok((Self::Add, ExprType::Label))
            }
            // TODO: support bitwise arithmetic between labels and integers
            (BinOpAst::BitAnd, ExprType::Boolean, ExprType::Boolean) => {
                Ok((Self::BitAnd, ExprType::Boolean))
            }
            (BinOpAst::BitAnd, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::BitAnd, ExprType::Integer))
            }
            (BinOpAst::BitOr, ExprType::Boolean, ExprType::Boolean) => {
                Ok((Self::BitOr, ExprType::Boolean))
            }
            (BinOpAst::BitOr, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::BitOr, ExprType::Integer))
            }
            (BinOpAst::BitXor, ExprType::Boolean, ExprType::Boolean) => {
                Ok((Self::BitXor, ExprType::Boolean))
            }
            (BinOpAst::BitXor, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::BitXor, ExprType::Integer))
            }
            // TODO: support byte selection between labels and integers
            (BinOpAst::Byte, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Byte, ExprType::Integer))
            }
            (BinOpAst::CmpEq, t1, t2)
                if let Some(t3) = t1.union(&t2)
                    && is_ordered_type(&t3) =>
            {
                Ok((Self::CmpEq, ExprType::Boolean))
            }
            (BinOpAst::CmpLe, t1, t2)
                if let Some(t3) = t1.union(&t2)
                    && is_ordered_type(&t3) =>
            {
                Ok((Self::CmpLe, ExprType::Boolean))
            }
            (BinOpAst::CmpLt, t1, t2)
                if let Some(t3) = t1.union(&t2)
                    && is_ordered_type(&t3) =>
            {
                Ok((Self::CmpLt, ExprType::Boolean))
            }
            (BinOpAst::CmpGe, t1, t2)
                if let Some(t3) = t1.union(&t2)
                    && is_ordered_type(&t3) =>
            {
                Ok((Self::CmpGe, ExprType::Boolean))
            }
            (BinOpAst::CmpGt, t1, t2)
                if let Some(t3) = t1.union(&t2)
                    && is_ordered_type(&t3) =>
            {
                Ok((Self::CmpGt, ExprType::Boolean))
            }
            (BinOpAst::CmpNe, t1, t2)
                if let Some(t3) = t1.union(&t2)
                    && is_ordered_type(&t3) =>
            {
                Ok((Self::CmpNe, ExprType::Boolean))
            }
            (BinOpAst::Concat, ExprType::List(t1), ExprType::List(t2))
                if let Some(t3) = t1.union(&t2) =>
            {
                Ok((Self::Concat, ExprType::List(Rc::new(t3))))
            }
            (BinOpAst::Concat, ExprType::String, ExprType::String) => {
                Ok((Self::Concat, ExprType::String))
            }
            (BinOpAst::Div, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Div, ExprType::Integer))
            }
            (BinOpAst::Mod, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Mod, ExprType::Integer))
            }
            (BinOpAst::Mul, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Mul, ExprType::Integer))
            }
            (BinOpAst::Pow, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Pow, ExprType::Integer))
            }
            (BinOpAst::Shl, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Shl, ExprType::Integer))
            }
            (BinOpAst::Shr, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Shr, ExprType::Integer))
            }
            (BinOpAst::Sub, ExprType::Integer, ExprType::Integer) => {
                Ok((Self::Sub, ExprType::Integer))
            }
            (BinOpAst::Sub, ExprType::Label, ExprType::Integer) => {
                Ok((Self::Sub, ExprType::Label))
            }
            (BinOpAst::Sub, ExprType::Label, ExprType::Label) => {
                Ok((Self::Sub, ExprType::Integer))
            }
            // Logical AND/OR are special-cased in `ExprCompiler`, and are
            // never passed to this method.
            (BinOpAst::LogAnd | BinOpAst::LogOr, _, _) => unreachable!(),
            (op, lhs_type, rhs_type) => {
                Err(Errs::one(ExprTypeError::CannotApplyBinaryOpToTypes {
                    op_span,
                    op,
                    lhs_span,
                    lhs_type,
                    rhs_span,
                    rhs_type,
                }))
            }
        }
    }

    pub(crate) fn evaluate(
        self,
        lhs: ExprValue,
        rhs: ExprValue,
    ) -> Result<ExprValue, ExprBinOpEvalError> {
        match self {
            Self::Add => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    Ok(ExprValue::Integer(lhs + rhs))
                }
                (ExprValue::Integer(int), ExprValue::Label(label))
                | (ExprValue::Label(label), ExprValue::Integer(int)) => {
                    Ok(ExprValue::Label(label + int))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::BitAnd => match (lhs, rhs) {
                (ExprValue::Boolean(lhs), ExprValue::Boolean(rhs)) => {
                    Ok(ExprValue::Boolean(lhs & rhs))
                }
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    Ok(ExprValue::Integer(lhs & rhs))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::BitOr => match (lhs, rhs) {
                (ExprValue::Boolean(lhs), ExprValue::Boolean(rhs)) => {
                    Ok(ExprValue::Boolean(lhs | rhs))
                }
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    Ok(ExprValue::Integer(lhs | rhs))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::BitXor => match (lhs, rhs) {
                (ExprValue::Boolean(lhs), ExprValue::Boolean(rhs)) => {
                    Ok(ExprValue::Boolean(lhs ^ rhs))
                }
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    Ok(ExprValue::Integer(lhs ^ rhs))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::Byte => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    if let Sign::Minus = rhs.sign() {
                        Err(ExprBinOpEvalError::ByteSelectByNegative(rhs))
                    } else {
                        let index = rhs.magnitude();
                        let bytes = lhs.to_signed_bytes_le();
                        let byte = if *index < BigUint::from(bytes.len()) {
                            bytes[usize::try_from(index).unwrap()]
                        } else if let Sign::Minus = lhs.sign() {
                            0xffu8
                        } else {
                            0x00u8
                        };
                        Ok(ExprValue::Integer(BigInt::from(byte)))
                    }
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::CmpEq => Ok(ExprValue::Boolean(
                compare_values(&lhs, &rhs)? == Ordering::Equal,
            )),
            Self::CmpLe => Ok(ExprValue::Boolean(
                compare_values(&lhs, &rhs)? != Ordering::Greater,
            )),
            Self::CmpLt => Ok(ExprValue::Boolean(
                compare_values(&lhs, &rhs)? == Ordering::Less,
            )),
            Self::CmpGe => Ok(ExprValue::Boolean(
                compare_values(&lhs, &rhs)? != Ordering::Less,
            )),
            Self::CmpGt => Ok(ExprValue::Boolean(
                compare_values(&lhs, &rhs)? == Ordering::Greater,
            )),
            Self::CmpNe => Ok(ExprValue::Boolean(
                compare_values(&lhs, &rhs)? != Ordering::Equal,
            )),
            Self::Concat => match (lhs, rhs) {
                (ExprValue::List(lhs), ExprValue::List(rhs)) => {
                    Ok(ExprValue::List(Rc::from([lhs, rhs].concat())))
                }
                (ExprValue::String(lhs), ExprValue::String(rhs)) => {
                    Ok(ExprValue::String(Rc::from([lhs, rhs].concat())))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::Div => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    if rhs.is_zero() {
                        Err(ExprBinOpEvalError::DivideByZero)
                    } else {
                        Ok(ExprValue::Integer(lhs.div_euclid(&rhs)))
                    }
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::Mod => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    if rhs.is_zero() {
                        Err(ExprBinOpEvalError::ModByZero)
                    } else {
                        Ok(ExprValue::Integer(lhs.rem_euclid(&rhs)))
                    }
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::Mul => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    Ok(ExprValue::Integer(lhs * rhs))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::Pow => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    if rhs.is_negative() {
                        Err(ExprBinOpEvalError::PowNegativeExponent(rhs))
                    } else {
                        Ok(ExprValue::Integer(lhs.pow(rhs.magnitude())))
                    }
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::Shl => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    let shift = Self::get_bit_shift_amount(rhs)?;
                    Ok(ExprValue::Integer(lhs << shift))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::Shr => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    let shift = Self::get_bit_shift_amount(rhs)?;
                    Ok(ExprValue::Integer(lhs >> shift))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
            Self::Sub => match (lhs, rhs) {
                (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                    Ok(ExprValue::Integer(lhs - rhs))
                }
                (ExprValue::Label(label), ExprValue::Integer(int)) => {
                    Ok(ExprValue::Label(label - int))
                }
                (ExprValue::Label(lhs), ExprValue::Label(rhs)) => {
                    Ok(ExprValue::Integer(subtract_labels(&lhs, &rhs)?))
                }
                _ => Err(ExprBinOpEvalError::InvalidType),
            },
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

/// Returns true if values of the given `ExprType` are comparable with
/// e.g. less/greater-than operators.
///
/// This returns `true` for `Bottom` and `Undefined`; since no concrete values
/// of those types exist, it is trivially true that such values are comparable,
/// and indeed it is legal to e.g. compare two expressions of type `Bottom`.
fn is_ordered_type(expr_type: &ExprType) -> bool {
    match expr_type {
        ExprType::Boolean => true,
        ExprType::Bottom => true,
        ExprType::Entity(_) => false,
        ExprType::Function(_) => false,
        ExprType::Integer => true,
        ExprType::Label => true,
        ExprType::List(item_type) => is_ordered_type(item_type),
        ExprType::String => true,
        ExprType::Tuple(item_types) => item_types.iter().all(is_ordered_type),
        ExprType::Undefined => true,
    }
}

/// Compares two expression values.
///
/// This will return an error if the values are of incompatible types, or if
/// they are of a non-ordered type (i.e. one for which `is_ordered_type`
/// returns false), or if they are labels in different address spaces, or they
/// are unresolved labels that cannot be compared yet.
fn compare_values(
    lhs_value: &ExprValue,
    rhs_value: &ExprValue,
) -> Result<Ordering, ExprBinOpEvalError> {
    match (lhs_value, rhs_value) {
        (ExprValue::Boolean(lhs), ExprValue::Boolean(rhs)) => Ok(lhs.cmp(rhs)),
        (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => Ok(lhs.cmp(rhs)),
        (ExprValue::Label(lhs), ExprValue::Label(rhs)) => {
            match subtract_labels(lhs, rhs)?.sign() {
                num_bigint::Sign::Minus => Ok(Ordering::Less),
                num_bigint::Sign::NoSign => Ok(Ordering::Equal),
                num_bigint::Sign::Plus => Ok(Ordering::Greater),
            }
        }
        (ExprValue::List(lhs_items), ExprValue::List(rhs_items)) => {
            for (lhs, rhs) in lhs_items.iter().zip(rhs_items.iter()) {
                match compare_values(lhs, rhs)? {
                    Ordering::Equal => continue,
                    other => return Ok(other),
                }
            }
            Ok(lhs_items.len().cmp(&rhs_items.len()))
        }
        (ExprValue::String(lhs), ExprValue::String(rhs)) => Ok(lhs.cmp(rhs)),
        (ExprValue::Tuple(lhs_items), ExprValue::Tuple(rhs_items)) => {
            if lhs_items.len() != rhs_items.len() {
                return Err(ExprBinOpEvalError::InvalidType);
            }
            for (lhs, rhs) in lhs_items.iter().zip(rhs_items.iter()) {
                match compare_values(lhs, rhs)? {
                    Ordering::Equal => continue,
                    other => return Ok(other),
                }
            }
            Ok(Ordering::Equal)
        }
        _ => Err(ExprBinOpEvalError::InvalidType),
    }
}

fn subtract_labels(
    lhs: &ExprLabel,
    rhs: &ExprLabel,
) -> Result<BigInt, ExprBinOpEvalError> {
    match (lhs, rhs) {
        (
            ExprLabel::AddrAbsolute { space: lhs_space, address: lhs_addr },
            ExprLabel::AddrAbsolute { space: rhs_space, address: rhs_addr },
        ) => {
            if lhs_space != rhs_space {
                Err(ExprBinOpEvalError::SubtractLabelsInDifferentAddrspaces(
                    lhs_space.clone(),
                    rhs_space.clone(),
                ))
            } else {
                Ok(lhs_addr - rhs_addr)
            }
        }
        (
            ExprLabel::ChunkAbsolute {
                chunk_index: lhs_index,
                address: lhs_addr,
            },
            ExprLabel::ChunkAbsolute {
                chunk_index: rhs_index,
                address: rhs_addr,
            },
        ) if lhs_index == rhs_index => Ok(lhs_addr - rhs_addr),
        (
            ExprLabel::ChunkRelative {
                chunk_index: lhs_index,
                offset: lhs_offset,
            },
            ExprLabel::ChunkRelative {
                chunk_index: rhs_index,
                offset: rhs_offset,
            },
        ) if lhs_index == rhs_index => Ok(lhs_offset - rhs_offset),
        (
            ExprLabel::SymbolRelative { name: lhs_name, offset: lhs_offset },
            ExprLabel::SymbolRelative { name: rhs_name, offset: rhs_offset },
        ) if lhs_name == rhs_name => Ok(lhs_offset - rhs_offset),
        _ => Err(ExprBinOpEvalError::SubtractLabelsUnresolved),
    }
}

impl BinaryIo for ExprBinOp {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_ADD => Ok(Self::Add),
            TAG_BIT_AND => Ok(Self::BitAnd),
            TAG_BIT_OR => Ok(Self::BitOr),
            TAG_BIT_XOR => Ok(Self::BitXor),
            TAG_BYTE => Ok(Self::Byte),
            TAG_CMP_EQ => Ok(Self::CmpEq),
            TAG_CMP_LE => Ok(Self::CmpLe),
            TAG_CMP_LT => Ok(Self::CmpLt),
            TAG_CMP_GE => Ok(Self::CmpGe),
            TAG_CMP_GT => Ok(Self::CmpGt),
            TAG_CMP_NE => Ok(Self::CmpNe),
            TAG_CONCAT => Ok(Self::Concat),
            TAG_DIV => Ok(Self::Div),
            TAG_MOD => Ok(Self::Mod),
            TAG_MUL => Ok(Self::Mul),
            TAG_POW => Ok(Self::Pow),
            TAG_SHL => Ok(Self::Shl),
            TAG_SHR => Ok(Self::Shr),
            TAG_SUB => Ok(Self::Sub),
            byte => Err(io::Error::other(format!(
                "unknown binop tag: 0x{:02x}",
                byte
            ))),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        let tag = match self {
            Self::Add => TAG_ADD,
            Self::BitAnd => TAG_BIT_AND,
            Self::BitOr => TAG_BIT_OR,
            Self::BitXor => TAG_BIT_XOR,
            Self::Byte => TAG_BYTE,
            Self::CmpEq => TAG_CMP_EQ,
            Self::CmpLe => TAG_CMP_LE,
            Self::CmpLt => TAG_CMP_LT,
            Self::CmpGe => TAG_CMP_GE,
            Self::CmpGt => TAG_CMP_GT,
            Self::CmpNe => TAG_CMP_NE,
            Self::Concat => TAG_CONCAT,
            Self::Div => TAG_DIV,
            Self::Mod => TAG_MOD,
            Self::Mul => TAG_MUL,
            Self::Pow => TAG_POW,
            Self::Shl => TAG_SHL,
            Self::Shr => TAG_SHR,
            Self::Sub => TAG_SUB,
        };
        tag.write_to(encoder)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ExprBinOp, ExprBinOpEvalError};
    use crate::expr::ExprValue;
    use crate::obj::assert_round_trips;
    use num_bigint::BigInt;

    fn int_value(value: i32) -> ExprValue {
        ExprValue::Integer(BigInt::from(value))
    }

    #[test]
    fn eval_byte_select_by_negative() {
        assert_eq!(
            ExprBinOp::Byte.evaluate(int_value(1), int_value(-1)),
            Err(ExprBinOpEvalError::ByteSelectByNegative(BigInt::from(-1)))
        );
    }

    #[test]
    fn eval_divide_by_zero() {
        assert_eq!(
            ExprBinOp::Div.evaluate(int_value(1), int_value(0)),
            Err(ExprBinOpEvalError::DivideByZero)
        );
    }

    #[test]
    fn eval_modulo_by_zero() {
        assert_eq!(
            ExprBinOp::Mod.evaluate(int_value(1), int_value(0)),
            Err(ExprBinOpEvalError::ModByZero)
        );
    }

    #[test]
    fn eval_pow_by_negative() {
        assert_eq!(
            ExprBinOp::Pow.evaluate(int_value(20), int_value(-5)),
            Err(ExprBinOpEvalError::PowNegativeExponent(BigInt::from(-5)))
        );
    }

    #[test]
    fn eval_shift_by_negative() {
        assert_eq!(
            ExprBinOp::Shl.evaluate(int_value(16), int_value(-2)),
            Err(ExprBinOpEvalError::BitShiftByNegative(BigInt::from(-2)))
        );
        assert_eq!(
            ExprBinOp::Shr.evaluate(int_value(16), int_value(-2)),
            Err(ExprBinOpEvalError::BitShiftByNegative(BigInt::from(-2)))
        );
    }

    #[test]
    fn binary_io_round_trip() {
        assert_round_trips(ExprBinOp::Add);
        assert_round_trips(ExprBinOp::BitAnd);
        assert_round_trips(ExprBinOp::BitOr);
        assert_round_trips(ExprBinOp::BitXor);
        assert_round_trips(ExprBinOp::Byte);
        assert_round_trips(ExprBinOp::CmpEq);
        assert_round_trips(ExprBinOp::CmpLe);
        assert_round_trips(ExprBinOp::CmpLt);
        assert_round_trips(ExprBinOp::CmpGe);
        assert_round_trips(ExprBinOp::CmpGt);
        assert_round_trips(ExprBinOp::CmpNe);
        assert_round_trips(ExprBinOp::Concat);
        assert_round_trips(ExprBinOp::Div);
        assert_round_trips(ExprBinOp::Mod);
        assert_round_trips(ExprBinOp::Mul);
        assert_round_trips(ExprBinOp::Pow);
        assert_round_trips(ExprBinOp::Shl);
        assert_round_trips(ExprBinOp::Shr);
        assert_round_trips(ExprBinOp::Sub);
    }
}

//===========================================================================//
