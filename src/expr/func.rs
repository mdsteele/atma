use super::error::ExprEvalError;
use super::value::ExprValue;
use crate::error::SrcSpan;
use crate::obj::BinaryIo;
use num_bigint::BigInt;
use std::fmt;
use std::io;

//===========================================================================//

const TAG_CBRTZ: u8 = 0;
const TAG_SQRTZ: u8 = 1;

//===========================================================================//

/// A built-in function that can be applied to an [`ExprValue`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ExprFunc {
    /// Integer cube root, rounding towards zero.
    Cbrtz,
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
            Self::Sqrtz => "%sqrtz",
        }
    }
}

impl BinaryIo for ExprFunc {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            TAG_CBRTZ => Ok(ExprFunc::Cbrtz),
            TAG_SQRTZ => Ok(ExprFunc::Sqrtz),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown function tag: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            Self::Cbrtz => TAG_CBRTZ.write_to(writer),
            Self::Sqrtz => TAG_SQRTZ.write_to(writer),
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
    pub fn into_expr_eval_error(self, arg_span: SrcSpan) -> ExprEvalError {
        match self {
            Self::InvalidArgumentType(_arg_value) => {
                ExprEvalError::InvalidType { span: arg_span }
            }
            Self::SquareRootOfNegative(arg_value) => {
                ExprEvalError::SquareRootOfNegative { arg_span, arg_value }
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
        assert_round_trips(ExprFunc::Sqrtz);
    }
}

//===========================================================================/
