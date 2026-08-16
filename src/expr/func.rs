use super::error::ExprEvalError;
use super::value::ExprValue;
use crate::error::SrcSpan;
use crate::obj::{BinaryIo, Decoder, Encoder};
use num_bigint::BigInt;
use std::fmt;
use std::io;
use std::rc::Rc;

//===========================================================================//

const TAG_CBRTZ: u8 = 0;
const TAG_ERROR: u8 = 1;
const TAG_SQRTZ: u8 = 2;

//===========================================================================//

/// A built-in function that can be applied to an [`ExprValue`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ExprFunc {
    /// Integer cube root, rounding towards zero.
    Cbrtz,
    /// Takes a string message and fails evaluation with that message.
    Error,
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
        match self {
            Self::Cbrtz => TAG_CBRTZ.write_to(encoder),
            Self::Error => TAG_ERROR.write_to(encoder),
            Self::Sqrtz => TAG_SQRTZ.write_to(encoder),
        }
    }
}

impl fmt::Display for ExprFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

//===========================================================================/

/// An error that can occur while calling an [ExprFunc] with an [ExprValue].
#[derive(Debug, Eq, PartialEq)]
pub enum ExprFuncEvalError {
    /// Called the `%error` function with the given message string.
    ErrorMessage(Rc<str>),
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
    /// Converts `self` into an [`ExprEvalError`].
    pub fn into_expr_eval_error(
        self,
        func_span: SrcSpan,
        arg_span: SrcSpan,
    ) -> ExprEvalError {
        match self {
            Self::ErrorMessage(message) => {
                let span = func_span.merged_with(arg_span);
                ExprEvalError::ErrorMessage { span, message }
            }
            Self::InvalidArgumentType(_arg_value) => {
                ExprEvalError::InvalidType { span: arg_span }
            }
            Self::SquareRootOfNegative(arg_value) => {
                let expr_span = func_span.merged_with(arg_span);
                ExprEvalError::SquareRootOfNegative {
                    expr_span,
                    arg_span,
                    arg_value,
                }
            }
        }
    }
}

//===========================================================================/

fn get_int(input: ExprValue) -> Result<BigInt, ExprFuncEvalError> {
    match input {
        ExprValue::Integer(bigint) => Ok(bigint),
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
        assert_round_trips(ExprFunc::Error);
        assert_round_trips(ExprFunc::Sqrtz);
    }
}

//===========================================================================/
