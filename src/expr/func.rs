use super::value::ExprValue;
use crate::error::{SourceError, SrcLoc};
use crate::obj::{BinaryIo, Decoder, Encoder};
use num_bigint::{BigInt, BigUint};
use num_integer::Integer;
use num_traits::{Euclid, Signed};
use std::cmp::Ordering;
use std::fmt;
use std::io;
use std::rc::Rc;

//===========================================================================//

const TAG_DIVC: u8 = 0;
const TAG_DIVF: u8 = 1;
const TAG_DIVR: u8 = 2;
const TAG_DIVU: u8 = 3;
const TAG_DIVX: u8 = 4;
const TAG_DIVZ: u8 = 5;
const TAG_ERROR: u8 = 6;
const TAG_LOG2C: u8 = 7;
const TAG_LOG2F: u8 = 8;
const TAG_LOG2X: u8 = 9;
const TAG_MODC: u8 = 10;
const TAG_MODF: u8 = 11;
const TAG_MODR: u8 = 12;
const TAG_MODU: u8 = 13;
const TAG_MODZ: u8 = 15;
const TAG_SQRTC: u8 = 16;
const TAG_SQRTF: u8 = 17;
const TAG_SQRTX: u8 = 18;

//===========================================================================//

/// A built-in function that can be applied to an [`ExprValue`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ExprFunc {
    // TODO: atan2 (integer atan2)
    // TODO: cos (integer cosine)
    /// Ceiling division; takes a pair of integers and divides the first by the
    /// second, rounding towards positive infinity.
    ///
    /// `%divc(a, b) * b + %modc(a, b) == a` for all `a` and (nonzero) `b`.
    Divc,
    /// Floor division; takes a pair of integers and divides the first by the
    /// second, rounding towards negative infinity.
    ///
    /// `%divf(a, b) * b + %modf(a, b) == a` for all `a` and (nonzero) `b`.
    Divf,
    /// Rounding division; takes a pair of integers and divides the first by
    /// the second, rounding towards the nearest integer (breaking ties by
    /// rounding away from zero).
    ///
    /// `%divr(a, b) * b + %modr(a, b) == a` for all `a` and (nonzero) `b`.
    Divr,
    /// Euclidian division; takes a pair of integers and divides the first by
    /// the second, rounding towards negative infinity if the divisor is
    /// positive, or towards positive infinity if the divisor is negative (so
    /// that the remainder is always non-negative).
    ///
    /// `%divu(a, b) * b + %modu(a, b) == a` for all `a` and (nonzero) `b`.
    Divu,
    /// Exact division; takes a pair of integers and divides the first by the
    /// second, failing evaluation if the remainder isn't zero.
    ///
    /// Note that there is no corresponding `%modx` function, since it would
    /// just always return zero (or fail).
    Divx,
    /// Truncating division; takes a pair of integers and divides the first by
    /// the second, rounding towards zero.
    ///
    /// `%divz(a, b) * b + %modz(a, b) == a` for all `a` and (nonzero) `b`.
    Divz,
    /// Takes a string message and fails evaluation with that message.
    Error,
    /// Ceiling of base-2 logarithm; takes an integer and returns its base-2
    /// logarithm, rounding towards positive infinity.
    Log2c,
    /// Floor of base-2 logarithm; takes an integer and returns its base-2
    /// logarithm, rounding towards negative infinity.
    Log2f,
    /// Exact base-2 logarithm; takes an integer and returns its base-2
    /// logarithm, failing evaluation if the integer isn't a power of 2.
    Log2x,
    /// Ceiling modulo; takes a pair of integers and returns the remainder of
    /// the first divided by the second, with the sign of the result being
    /// opposite the sign of the divisor.
    ///
    /// `%divc(a, b) * b + %modc(a, b) == a` for all `a` and (nonzero) `b`.
    Modc,
    /// Floor modulo; takes a pair of integers and returns the remainder of the
    /// first divided by the second, with the sign of the result matching the
    /// sign of the divisor.
    ///
    /// `%divf(a, b) * b + %modf(a, b) == a` for all `a` and (nonzero) `b`.
    Modf,
    /// Rounding modulo; takes a pair of integers and returns the remainder of
    /// the first divided by the second when the quotient is rounded to the
    /// nearest integer (breaking ties by rounding away from zero).  Thus, the
    /// result will always be in the range `-b/2..=b/2`, where `b` is the
    /// divisor.
    ///
    /// `%divr(a, b) * b + %modr(a, b) == a` for all `a` and (nonzero) `b`.
    Modr,
    /// Euclidian modulo; takes a pair of integers and returns the remainder of
    /// the first divided by the second, with the sign of the result always
    /// non-negative.
    ///
    /// `%divu(a, b) * b + %modu(a, b) == a` for all `a` and (nonzero) `b`.
    Modu,
    /// Truncating modulo; takes a pair of integers and returns the remainder
    /// of the first divided by the second, with the sign of the result
    /// matching the sign of the dividend.
    ///
    /// `%divz(a, b) * b + %modz(a, b) == a` for all `a` and (nonzero) `b`.
    Modz,
    // TODO: sin (integer sine)
    /// Ceiling of square root; computes the square root of the integer
    /// argument, rounding towards positive infinity.
    Sqrtc,
    /// Floor of square root; computes the square root of the integer
    /// argument, rounding towards negative infinity.
    Sqrtf,
    // TODO: sqrtr (round-to-nearest)
    /// Exact square root; computes the square root of the integer argument,
    /// failing evaluation if the argument isn't a square number.
    Sqrtx,
}

impl ExprFunc {
    /// Calls this function on the given argument.
    pub fn call(
        &self,
        arg: ExprValue,
    ) -> Result<ExprValue, ExprFuncEvalError> {
        match self {
            Self::Divc => {
                let (lhs, rhs) = get_div_pair(arg)?;
                Ok(ExprValue::Integer(lhs.div_ceil(&rhs)))
            }
            Self::Divf => {
                let (lhs, rhs) = get_div_pair(arg)?;
                Ok(ExprValue::Integer(lhs.div_floor(&rhs)))
            }
            Self::Divr => {
                let (mut lhs, rhs) = get_div_pair(arg)?;
                lhs <<= 1;
                let mut quotient = lhs / rhs;
                if quotient > BigInt::ZERO {
                    quotient.inc();
                }
                quotient >>= 1;
                Ok(ExprValue::Integer(quotient))
            }
            Self::Divu => {
                let (lhs, rhs) = get_div_pair(arg)?;
                Ok(ExprValue::Integer(lhs.div_euclid(&rhs)))
            }
            Self::Divx => {
                let (lhs, rhs) = get_div_pair(arg)?;
                let (quot, rem) = lhs.div_rem(&rhs);
                if rem == BigInt::ZERO {
                    Ok(ExprValue::Integer(quot))
                } else {
                    Err(ExprFuncEvalError::InexactDivision(lhs, rhs))
                }
            }
            Self::Divz => {
                let (lhs, rhs) = get_div_pair(arg)?;
                Ok(ExprValue::Integer(lhs / rhs))
            }
            Self::Error => Err(ExprFuncEvalError::ErrorMessage(get_str(arg)?)),
            Self::Log2c => {
                let mut arg = get_log_arg(arg)?;
                arg.dec();
                Ok(ExprValue::Integer(BigInt::from(arg.bits())))
            }
            Self::Log2f => {
                let arg = get_log_arg(arg)?;
                Ok(ExprValue::Integer(BigInt::from(arg.bits() - 1)))
            }
            Self::Log2x => {
                let arg = get_log_arg(arg)?;
                if arg.count_ones() != 1 {
                    Err(ExprFuncEvalError::InexactLogarithm(2, arg))
                } else {
                    Ok(ExprValue::Integer(BigInt::from(arg.bits() - 1)))
                }
            }
            Self::Modc => {
                let (lhs, rhs) = get_mod_pair(arg)?;
                Ok(ExprValue::Integer(lhs.mod_floor(&-rhs)))
            }
            Self::Modf => {
                let (lhs, rhs) = get_mod_pair(arg)?;
                Ok(ExprValue::Integer(lhs.mod_floor(&rhs)))
            }
            Self::Modr => {
                let (lhs, rhs) = get_mod_pair(arg)?;
                let rhs_abs = rhs.abs();
                let mut remainder = lhs.mod_floor(&rhs_abs);
                let ordering = rhs_abs.cmp(&(&remainder << 1));
                if ordering == Ordering::Less
                    || (ordering == Ordering::Equal && lhs > BigInt::ZERO)
                {
                    remainder -= rhs_abs;
                }
                Ok(ExprValue::Integer(remainder))
            }
            Self::Modu => {
                let (lhs, rhs) = get_mod_pair(arg)?;
                Ok(ExprValue::Integer(lhs.rem_euclid(&rhs)))
            }
            Self::Modz => {
                let (lhs, rhs) = get_mod_pair(arg)?;
                Ok(ExprValue::Integer(lhs % rhs))
            }
            Self::Sqrtc => {
                let mut arg = get_sqrt_arg(arg)?;
                if arg == BigUint::ZERO {
                    Ok(ExprValue::Integer(BigInt::ZERO))
                } else {
                    arg.dec();
                    let mut sqrt = arg.sqrt();
                    sqrt.inc();
                    Ok(ExprValue::Integer(BigInt::from(sqrt)))
                }
            }
            Self::Sqrtf => {
                let arg = get_sqrt_arg(arg)?;
                Ok(ExprValue::Integer(BigInt::from(arg.sqrt())))
            }
            Self::Sqrtx => {
                let arg = get_sqrt_arg(arg)?;
                let sqrt = arg.sqrt();
                if &sqrt * &sqrt == arg {
                    Ok(ExprValue::Integer(BigInt::from(sqrt)))
                } else {
                    Err(ExprFuncEvalError::InexactSquareRoot(arg))
                }
            }
        }
    }

    /// Returns the identifier name of this built-in function.
    pub fn name(&self) -> &'static str {
        match self {
            Self::Divc => "%divc",
            Self::Divf => "%divf",
            Self::Divr => "%divr",
            Self::Divu => "%divu",
            Self::Divx => "%divx",
            Self::Divz => "%divz",
            Self::Error => "%error",
            Self::Log2c => "%log2c",
            Self::Log2f => "%log2f",
            Self::Log2x => "%log2x",
            Self::Modc => "%modc",
            Self::Modf => "%modf",
            Self::Modr => "%modr",
            Self::Modu => "%modu",
            Self::Modz => "%modz",
            Self::Sqrtc => "%sqrtc",
            Self::Sqrtf => "%sqrtf",
            Self::Sqrtx => "%sqrtx",
        }
    }
}

impl BinaryIo for ExprFunc {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_DIVC => Ok(ExprFunc::Divc),
            TAG_DIVF => Ok(ExprFunc::Divf),
            TAG_DIVR => Ok(ExprFunc::Divr),
            TAG_DIVU => Ok(ExprFunc::Divu),
            TAG_DIVX => Ok(ExprFunc::Divx),
            TAG_DIVZ => Ok(ExprFunc::Divz),
            TAG_ERROR => Ok(ExprFunc::Error),
            TAG_LOG2C => Ok(ExprFunc::Log2c),
            TAG_LOG2F => Ok(ExprFunc::Log2f),
            TAG_LOG2X => Ok(ExprFunc::Log2x),
            TAG_MODC => Ok(ExprFunc::Modc),
            TAG_MODF => Ok(ExprFunc::Modf),
            TAG_MODR => Ok(ExprFunc::Modr),
            TAG_MODU => Ok(ExprFunc::Modu),
            TAG_MODZ => Ok(ExprFunc::Modz),
            TAG_SQRTC => Ok(ExprFunc::Sqrtc),
            TAG_SQRTF => Ok(ExprFunc::Sqrtf),
            TAG_SQRTX => Ok(ExprFunc::Sqrtx),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown function tag: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        let tag = match self {
            Self::Divc => TAG_DIVC,
            Self::Divf => TAG_DIVF,
            Self::Divr => TAG_DIVR,
            Self::Divu => TAG_DIVU,
            Self::Divx => TAG_DIVX,
            Self::Divz => TAG_DIVZ,
            Self::Error => TAG_ERROR,
            Self::Log2c => TAG_LOG2C,
            Self::Log2f => TAG_LOG2F,
            Self::Log2x => TAG_LOG2X,
            Self::Modc => TAG_MODC,
            Self::Modf => TAG_MODF,
            Self::Modr => TAG_MODR,
            Self::Modu => TAG_MODU,
            Self::Modz => TAG_MODZ,
            Self::Sqrtc => TAG_SQRTC,
            Self::Sqrtf => TAG_SQRTF,
            Self::Sqrtx => TAG_SQRTX,
        };
        tag.write_to(encoder)
    }
}

impl fmt::Display for ExprFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

//===========================================================================/

/// An error that can occur while calling an [ExprFunc] with an [ExprValue].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprFuncEvalError {
    /// Tried to divide an integer, but the divisor was zero.
    DivideByZero,
    /// Called the `%error` function with the given message string.
    ErrorMessage(Rc<str>),
    /// Requested an exact division result, but the dividend is not a multiple
    /// of the divisor.
    InexactDivision(BigInt, BigInt),
    /// Requested an exact logarithm result, but the argument is not a power of
    /// the base.
    InexactLogarithm(u8, BigUint),
    /// Requested an exact square root result, but the argument is not a square
    /// number.
    InexactSquareRoot(BigUint),
    /// Received a value of the wrong type.
    ///
    /// This shouldn't normally happen unless an object file has been
    /// corrupted, since ATMA normally performs static typechecking before
    /// evaluation.
    InvalidArgumentType(ExprValue),
    /// Tried to calculate the logarithm of a non-positive number.
    LogarithmOfNonPositive(BigInt),
    /// Tried to mod an integer, but the modulus was zero.
    ModuloByZero,
    /// Tried to calculate the square root of a negative number.
    SquareRootOfNegative(BigInt),
}

impl ExprFuncEvalError {
    /// Converts the error into a `SourceError`.
    pub fn to_source_error(self, arg_loc: SrcLoc) -> SourceError {
        match self {
            Self::DivideByZero => {
                let message = "divisor cannot be zero";
                SourceError::new(arg_loc, message).with_primary_label("")
            }
            Self::ErrorMessage(message) => {
                SourceError::new(arg_loc, message).with_primary_label("")
            }
            Self::InexactDivision(dividend, divisor) => {
                let message = format!(
                    "quotient is inexact: {dividend} is not a multiple of \
                     {divisor}"
                );
                // TODO: add hint about other division functions
                SourceError::new(arg_loc, message).with_primary_label("")
            }
            Self::InexactLogarithm(base, argument) => {
                let message = format!(
                    "logarithm is inexact: {argument} is not a power of \
                     {base}"
                );
                // TODO: add hint about other logarithm functions
                SourceError::new(arg_loc, message).with_primary_label("")
            }
            Self::InexactSquareRoot(argument) => {
                let message = format!(
                    "square root is inexact: {argument} is not a square number"
                );
                // TODO: add hint about other square root functions
                SourceError::new(arg_loc, message).with_primary_label("")
            }
            Self::InvalidArgumentType(_arg_value) => {
                SourceError::new(arg_loc, "invalid argument type")
                    .with_primary_label("")
            }
            Self::LogarithmOfNonPositive(arg_value) => {
                let message = "logarithm argument must be greater than zero";
                let label = format!("this evaluates to {arg_value}");
                SourceError::new(arg_loc, message).with_primary_label(label)
            }
            Self::ModuloByZero => {
                let message = "modulus cannot be zero";
                SourceError::new(arg_loc, message).with_primary_label("")
            }
            Self::SquareRootOfNegative(arg_value) => {
                let message = "square root argument must be non-negative";
                let label = format!("this evaluates to {arg_value}");
                SourceError::new(arg_loc, message).with_primary_label(label)
            }
        }
    }
}

//===========================================================================/

fn get_div_pair(
    input: ExprValue,
) -> Result<(BigInt, BigInt), ExprFuncEvalError> {
    let (lhs, rhs) = get_int_pair(input)?;
    if rhs == BigInt::ZERO {
        return Err(ExprFuncEvalError::DivideByZero);
    }
    Ok((lhs, rhs))
}

fn get_int(input: ExprValue) -> Result<BigInt, ExprFuncEvalError> {
    match input {
        ExprValue::Integer(bigint) => Ok(bigint),
        other => Err(ExprFuncEvalError::InvalidArgumentType(other)),
    }
}

fn get_int_pair(
    input: ExprValue,
) -> Result<(BigInt, BigInt), ExprFuncEvalError> {
    match input {
        ExprValue::Tuple(items) => {
            if let [ExprValue::Integer(first), ExprValue::Integer(second)] =
                Rc::as_ref(&items)
            {
                Ok((first.clone(), second.clone()))
            } else {
                Err(ExprFuncEvalError::InvalidArgumentType(ExprValue::Tuple(
                    items,
                )))
            }
        }
        other => Err(ExprFuncEvalError::InvalidArgumentType(other)),
    }
}

fn get_log_arg(input: ExprValue) -> Result<BigUint, ExprFuncEvalError> {
    let arg = get_int(input)?;
    if arg <= BigInt::ZERO {
        return Err(ExprFuncEvalError::LogarithmOfNonPositive(arg));
    }
    Ok(arg.into_parts().1)
}

fn get_mod_pair(
    input: ExprValue,
) -> Result<(BigInt, BigInt), ExprFuncEvalError> {
    let (lhs, rhs) = get_int_pair(input)?;
    if rhs == BigInt::ZERO {
        return Err(ExprFuncEvalError::ModuloByZero);
    }
    Ok((lhs, rhs))
}

fn get_sqrt_arg(input: ExprValue) -> Result<BigUint, ExprFuncEvalError> {
    let arg = get_int(input)?;
    if arg < BigInt::ZERO {
        return Err(ExprFuncEvalError::SquareRootOfNegative(arg));
    }
    Ok(arg.into_parts().1)
}

fn get_str(input: ExprValue) -> Result<Rc<str>, ExprFuncEvalError> {
    match input {
        ExprValue::String(string) => Ok(string),
        other => Err(ExprFuncEvalError::InvalidArgumentType(other)),
    }
}

//===========================================================================/

#[cfg(test)]
mod tests {
    use super::{ExprFunc, ExprFuncEvalError};
    use crate::expr::ExprValue;
    use crate::obj::assert_round_trips;
    use num_bigint::{BigInt, BigUint};
    use std::rc::Rc;

    fn int_value(value: i32) -> ExprValue {
        ExprValue::Integer(BigInt::from(value))
    }

    fn int_pair(first: i32, second: i32) -> ExprValue {
        ExprValue::Tuple(Rc::from([int_value(first), int_value(second)]))
    }

    fn str_value(value: &str) -> ExprValue {
        ExprValue::String(Rc::from(value))
    }

    #[test]
    fn call_divc_func() {
        let func = ExprFunc::Divc;
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(3)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(3)));
    }

    #[test]
    fn call_divf_func() {
        let func = ExprFunc::Divf;
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(-3)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(-3)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(2)));
    }

    #[test]
    fn call_divr_func() {
        let func = ExprFunc::Divr;
        assert_eq!(func.call(int_pair(4, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(8, 3)), Ok(int_value(3)));
        assert_eq!(func.call(int_pair(-4, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-8, 3)), Ok(int_value(-3)));
        assert_eq!(func.call(int_pair(4, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(8, -3)), Ok(int_value(-3)));
        assert_eq!(func.call(int_pair(-4, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-8, -3)), Ok(int_value(3)));
        // Break ties by rounding quotient away from zero:
        assert_eq!(func.call(int_pair(5, 2)), Ok(int_value(3)));
        assert_eq!(func.call(int_pair(-5, 2)), Ok(int_value(-3)));
        assert_eq!(func.call(int_pair(5, -2)), Ok(int_value(-3)));
        assert_eq!(func.call(int_pair(-5, -2)), Ok(int_value(3)));
    }

    #[test]
    fn call_divu_func() {
        let func = ExprFunc::Divu;
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(-3)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(3)));
    }

    #[test]
    fn call_divx_func() {
        let func = ExprFunc::Divx;
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(2)));
        assert_eq!(
            func.call(int_pair(5, 3)),
            Err(ExprFuncEvalError::InexactDivision(
                BigInt::from(5),
                BigInt::from(3)
            ))
        );
    }

    #[test]
    fn call_divz_func() {
        let func = ExprFunc::Divz;
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(2)));
    }

    #[test]
    fn division_errors() {
        for func in [
            ExprFunc::Divc,
            ExprFunc::Divf,
            ExprFunc::Divr,
            ExprFunc::Divu,
            ExprFunc::Divx,
            ExprFunc::Divz,
        ] {
            assert_eq!(
                func.call(int_pair(37, 0)),
                Err(ExprFuncEvalError::DivideByZero)
            );
            assert_eq!(
                func.call(int_value(3)),
                Err(ExprFuncEvalError::InvalidArgumentType(int_value(3)))
            );
        }
    }

    #[test]
    fn call_error_func() {
        let func = ExprFunc::Error;
        assert_eq!(
            func.call(str_value("foobar")),
            Err(ExprFuncEvalError::ErrorMessage(Rc::from("foobar")))
        );
        assert_eq!(
            func.call(int_value(0)),
            Err(ExprFuncEvalError::InvalidArgumentType(int_value(0)))
        );
    }

    #[test]
    fn call_log2c_func() {
        let func = ExprFunc::Log2c;
        assert_eq!(func.call(int_value(1)), Ok(int_value(0)));
        assert_eq!(func.call(int_value(2)), Ok(int_value(1)));
        assert_eq!(func.call(int_value(3)), Ok(int_value(2)));
        assert_eq!(func.call(int_value(4)), Ok(int_value(2)));
        assert_eq!(func.call(int_value(5)), Ok(int_value(3)));
    }

    #[test]
    fn call_log2f_func() {
        let func = ExprFunc::Log2f;
        assert_eq!(func.call(int_value(1)), Ok(int_value(0)));
        assert_eq!(func.call(int_value(2)), Ok(int_value(1)));
        assert_eq!(func.call(int_value(3)), Ok(int_value(1)));
        assert_eq!(func.call(int_value(4)), Ok(int_value(2)));
        assert_eq!(func.call(int_value(5)), Ok(int_value(2)));
    }

    #[test]
    fn call_log2x_func() {
        let func = ExprFunc::Log2x;
        assert_eq!(func.call(int_value(1)), Ok(int_value(0)));
        assert_eq!(func.call(int_value(2)), Ok(int_value(1)));
        assert_eq!(func.call(int_value(4)), Ok(int_value(2)));
        assert_eq!(func.call(int_value(0x400)), Ok(int_value(10)));
        assert_eq!(
            func.call(int_value(3)),
            Err(ExprFuncEvalError::InexactLogarithm(2, BigUint::from(3u32)))
        );
    }

    #[test]
    fn logarithm_errors() {
        for func in [ExprFunc::Log2c, ExprFunc::Log2f, ExprFunc::Log2x] {
            assert_eq!(
                func.call(int_value(0)),
                Err(ExprFuncEvalError::LogarithmOfNonPositive(BigInt::ZERO))
            );
            assert_eq!(
                func.call(int_value(-3)),
                Err(ExprFuncEvalError::LogarithmOfNonPositive(BigInt::from(
                    -3
                )))
            );
            assert_eq!(
                func.call(str_value("0")),
                Err(ExprFuncEvalError::InvalidArgumentType(str_value("0")))
            );
        }
    }

    #[test]
    fn call_modc_func() {
        let func = ExprFunc::Modc;
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(2)));
    }

    #[test]
    fn call_modf_func() {
        let func = ExprFunc::Modf;
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(-1)));
    }

    #[test]
    fn call_modr_func() {
        let func = ExprFunc::Modr;
        assert_eq!(func.call(int_pair(4, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(8, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-4, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-8, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(4, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(8, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-4, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-8, -3)), Ok(int_value(1)));
        // Break ties by rounding quotient away from zero:
        assert_eq!(func.call(int_pair(5, 2)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-5, 2)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(5, -2)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(-5, -2)), Ok(int_value(1)));
    }

    #[test]
    fn call_modu_func() {
        let func = ExprFunc::Modu;
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(2)));
    }

    #[test]
    fn call_modz_func() {
        let func = ExprFunc::Modz;
        assert_eq!(func.call(int_pair(5, 3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, 3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-5, 3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-6, 3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, 3)), Ok(int_value(-1)));
        assert_eq!(func.call(int_pair(5, -3)), Ok(int_value(2)));
        assert_eq!(func.call(int_pair(6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(7, -3)), Ok(int_value(1)));
        assert_eq!(func.call(int_pair(-5, -3)), Ok(int_value(-2)));
        assert_eq!(func.call(int_pair(-6, -3)), Ok(int_value(0)));
        assert_eq!(func.call(int_pair(-7, -3)), Ok(int_value(-1)));
    }

    #[test]
    fn modulo_errors() {
        for func in [
            ExprFunc::Modc,
            ExprFunc::Modf,
            ExprFunc::Modr,
            ExprFunc::Modu,
            ExprFunc::Modz,
        ] {
            assert_eq!(
                func.call(int_pair(37, 0)),
                Err(ExprFuncEvalError::ModuloByZero)
            );
            assert_eq!(
                func.call(int_value(3)),
                Err(ExprFuncEvalError::InvalidArgumentType(int_value(3)))
            );
        }
    }

    #[test]
    fn call_sqrtc_func() {
        let func = ExprFunc::Sqrtc;
        assert_eq!(func.call(int_value(0)), Ok(int_value(0)));
        assert_eq!(func.call(int_value(1)), Ok(int_value(1)));
        assert_eq!(func.call(int_value(2)), Ok(int_value(2)));
        assert_eq!(func.call(int_value(24)), Ok(int_value(5)));
        assert_eq!(func.call(int_value(25)), Ok(int_value(5)));
        assert_eq!(func.call(int_value(26)), Ok(int_value(6)));
    }

    #[test]
    fn call_sqrtf_func() {
        let func = ExprFunc::Sqrtf;
        assert_eq!(func.call(int_value(0)), Ok(int_value(0)));
        assert_eq!(func.call(int_value(1)), Ok(int_value(1)));
        assert_eq!(func.call(int_value(2)), Ok(int_value(1)));
        assert_eq!(func.call(int_value(24)), Ok(int_value(4)));
        assert_eq!(func.call(int_value(25)), Ok(int_value(5)));
        assert_eq!(func.call(int_value(26)), Ok(int_value(5)));
    }

    #[test]
    fn call_sqrtx_func() {
        let func = ExprFunc::Sqrtx;
        assert_eq!(func.call(int_value(0)), Ok(int_value(0)));
        assert_eq!(func.call(int_value(1)), Ok(int_value(1)));
        assert_eq!(func.call(int_value(25)), Ok(int_value(5)));
        assert_eq!(
            func.call(int_value(2)),
            Err(ExprFuncEvalError::InexactSquareRoot(BigUint::from(2u32)))
        );
    }

    #[test]
    fn square_root_errors() {
        for func in [ExprFunc::Sqrtc, ExprFunc::Sqrtf, ExprFunc::Sqrtx] {
            assert_eq!(
                func.call(int_value(-9)),
                Err(ExprFuncEvalError::SquareRootOfNegative(BigInt::from(-9)))
            );
            assert_eq!(
                func.call(str_value("0")),
                Err(ExprFuncEvalError::InvalidArgumentType(str_value("0")))
            );
        }
    }

    #[test]
    fn round_trips() {
        assert_round_trips(ExprFunc::Divc);
        assert_round_trips(ExprFunc::Divf);
        assert_round_trips(ExprFunc::Divr);
        assert_round_trips(ExprFunc::Divu);
        assert_round_trips(ExprFunc::Divx);
        assert_round_trips(ExprFunc::Divz);
        assert_round_trips(ExprFunc::Error);
        assert_round_trips(ExprFunc::Log2c);
        assert_round_trips(ExprFunc::Log2f);
        assert_round_trips(ExprFunc::Log2x);
        assert_round_trips(ExprFunc::Modc);
        assert_round_trips(ExprFunc::Modf);
        assert_round_trips(ExprFunc::Modr);
        assert_round_trips(ExprFunc::Modu);
        assert_round_trips(ExprFunc::Modz);
        assert_round_trips(ExprFunc::Sqrtc);
        assert_round_trips(ExprFunc::Sqrtf);
        assert_round_trips(ExprFunc::Sqrtx);
    }
}

//===========================================================================/
