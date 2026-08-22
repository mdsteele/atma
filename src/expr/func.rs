use super::value::ExprValue;
use crate::error::{SourceError, SrcLoc};
use crate::obj::{BinaryIo, Decoder, Encoder};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::Euclid;
use std::fmt;
use std::io;
use std::rc::Rc;

//===========================================================================//

const TAG_CBRTZ: u8 = 0;
const TAG_DIVC: u8 = 1;
const TAG_DIVF: u8 = 2;
const TAG_DIVU: u8 = 3;
const TAG_DIVX: u8 = 4;
const TAG_DIVZ: u8 = 5;
const TAG_ERROR: u8 = 6;
const TAG_SQRTZ: u8 = 7;

//===========================================================================//

/// A built-in function that can be applied to an [`ExprValue`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ExprFunc {
    // TODO: atan2 (integer atan2)
    // TODO: cbrtx (exact cube root)
    /// Integer cube root, rounding towards zero.
    Cbrtz,
    // TODO: cos (integer cosine)
    /// Ceiling division; takes a pair of integers and divides the first by the
    /// second, rounding towards positive infinity.
    Divc,
    /// Floor division; takes a pair of integers and divides the first by the
    /// second, rounding towards negative infinity.
    Divf,
    /// Euclidian division; takes a pair of integers and divides the first by
    /// the second, rounding towards negative infinity if the divisor is
    /// positive, or towards positive infinity if the divisor is negative.
    Divu,
    /// Exact division; takes a pair of integers and divides the first by the
    /// second, failing evaluation if the remainder isn't zero.
    Divx,
    /// Truncating division; takes a pair of integers and divides the first by
    /// the second, rounding towards zero.
    Divz,
    /// Takes a string message and fails evaluation with that message.
    Error,
    // TODO: log2c (ceiling of base-2 logarithm)
    // TODO: log2f (floor of base-2 logarithm)
    // TODO: log2x (exact base-2 logarithm)
    // TODO: modc (ceiling modulo)
    // TODO: modf (floor modulo)
    // TODO: modu (Euclidian modulo)
    // TODO: modz (truncating modulo)
    // TODO: sin (integer sine)
    // TODO: sqrtc (ceiling of square root)
    // TODO: sqrtx (exact square root)
    /// Integer square root, rounding towards zero.
    Sqrtz,
}

impl ExprFunc {
    /// Calls this function on the given argument.
    pub fn call(
        &self,
        arg: ExprValue,
    ) -> Result<ExprValue, ExprFuncEvalError> {
        match self {
            Self::Cbrtz => Ok(ExprValue::Integer(get_int(arg)?.cbrt())),
            Self::Divc => {
                let (lhs, rhs) = get_div_pair(arg)?;
                Ok(ExprValue::Integer(lhs.div_ceil(&rhs)))
            }
            Self::Divf => {
                let (lhs, rhs) = get_div_pair(arg)?;
                Ok(ExprValue::Integer(lhs.div_floor(&rhs)))
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
            Self::Sqrtz => {
                let arg = get_int(arg)?;
                if arg >= BigInt::ZERO {
                    Ok(ExprValue::Integer(arg.sqrt()))
                } else {
                    Err(ExprFuncEvalError::SquareRootOfNegative(arg))
                }
            }
        }
    }

    /// Returns the identifier name of this built-in function.
    pub fn name(&self) -> &'static str {
        match self {
            Self::Cbrtz => "%cbrtz",
            Self::Divc => "%divc",
            Self::Divf => "%divf",
            Self::Divu => "%divu",
            Self::Divx => "%divx",
            Self::Divz => "%divz",
            Self::Error => "%error",
            Self::Sqrtz => "%sqrtz",
        }
    }
}

impl BinaryIo for ExprFunc {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_CBRTZ => Ok(ExprFunc::Cbrtz),
            TAG_DIVC => Ok(ExprFunc::Divc),
            TAG_DIVF => Ok(ExprFunc::Divf),
            TAG_DIVU => Ok(ExprFunc::Divu),
            TAG_DIVX => Ok(ExprFunc::Divx),
            TAG_DIVZ => Ok(ExprFunc::Divz),
            TAG_ERROR => Ok(ExprFunc::Error),
            TAG_SQRTZ => Ok(ExprFunc::Sqrtz),
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
            Self::Cbrtz => TAG_CBRTZ,
            Self::Divc => TAG_DIVC,
            Self::Divf => TAG_DIVF,
            Self::Divu => TAG_DIVU,
            Self::Divx => TAG_DIVX,
            Self::Divz => TAG_DIVZ,
            Self::Error => TAG_ERROR,
            Self::Sqrtz => TAG_SQRTZ,
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
    /// Received a value of the wrong type.
    ///
    /// This shouldn't normally happen unless an object file has been
    /// corrupted, since ATMA normally performs static typechecking before
    /// evaluation.
    InvalidArgumentType(ExprValue),
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
            Self::InvalidArgumentType(_arg_value) => {
                SourceError::new(arg_loc, "invalid argument type")
                    .with_primary_label("")
            }
            Self::SquareRootOfNegative(arg_value) => {
                let message = "square root argument must be non-negative";
                let label =
                    format!("the value of this expression is {arg_value}");
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
        Err(ExprFuncEvalError::DivideByZero)
    } else {
        Ok((lhs, rhs))
    }
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
    use num_bigint::BigInt;
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
    fn call_cbrtz() {
        let func = ExprFunc::Cbrtz;
        assert_eq!(func.call(int_value(0)), Ok(int_value(0)));
        assert_eq!(func.call(int_value(63)), Ok(int_value(3)));
        assert_eq!(func.call(int_value(64)), Ok(int_value(4)));
        assert_eq!(func.call(int_value(65)), Ok(int_value(4)));
        assert_eq!(func.call(int_value(-63)), Ok(int_value(-3)));
        assert_eq!(func.call(int_value(-64)), Ok(int_value(-4)));
        assert_eq!(func.call(int_value(-65)), Ok(int_value(-4)));
        assert_eq!(
            func.call(str_value("0")),
            Err(ExprFuncEvalError::InvalidArgumentType(str_value("0")))
        );
    }

    #[test]
    fn call_divc() {
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
        assert_eq!(
            func.call(int_value(3)),
            Err(ExprFuncEvalError::InvalidArgumentType(int_value(3)))
        );
    }

    #[test]
    fn call_divf() {
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
        assert_eq!(
            func.call(int_value(3)),
            Err(ExprFuncEvalError::InvalidArgumentType(int_value(3)))
        );
    }

    #[test]
    fn call_divu() {
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
        assert_eq!(
            func.call(int_value(3)),
            Err(ExprFuncEvalError::InvalidArgumentType(int_value(3)))
        );
    }

    #[test]
    fn call_divx() {
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
        assert_eq!(
            func.call(int_value(3)),
            Err(ExprFuncEvalError::InvalidArgumentType(int_value(3)))
        );
    }

    #[test]
    fn call_divz() {
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
        assert_eq!(
            func.call(int_value(3)),
            Err(ExprFuncEvalError::InvalidArgumentType(int_value(3)))
        );
    }

    #[test]
    fn call_error() {
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
    fn call_sqrtz() {
        let func = ExprFunc::Sqrtz;
        assert_eq!(func.call(int_value(0)), Ok(int_value(0)));
        assert_eq!(func.call(int_value(24)), Ok(int_value(4)));
        assert_eq!(func.call(int_value(25)), Ok(int_value(5)));
        assert_eq!(func.call(int_value(26)), Ok(int_value(5)));
        assert_eq!(
            func.call(int_value(-25)),
            Err(ExprFuncEvalError::SquareRootOfNegative(BigInt::from(-25)))
        );
        assert_eq!(
            func.call(str_value("0")),
            Err(ExprFuncEvalError::InvalidArgumentType(str_value("0")))
        );
    }

    #[test]
    fn round_trips() {
        assert_round_trips(ExprFunc::Cbrtz);
        assert_round_trips(ExprFunc::Divc);
        assert_round_trips(ExprFunc::Divf);
        assert_round_trips(ExprFunc::Divu);
        assert_round_trips(ExprFunc::Divx);
        assert_round_trips(ExprFunc::Divz);
        assert_round_trips(ExprFunc::Error);
        assert_round_trips(ExprFunc::Sqrtz);
    }
}

//===========================================================================/
