use crate::obj::BinaryIo;
use num_bigint::BigInt;
use std::fmt;
use std::io;
use std::rc::Rc;

//===========================================================================//

const TAG_FALSE: u8 = 0;
const TAG_TRUE: u8 = 1;
const TAG_INTEGER: u8 = 2;
const TAG_LIST: u8 = 3;
const TAG_STRING: u8 = 4;
const TAG_TUPLE: u8 = 5;

//===========================================================================//

/// Represents the type of an [`AdsValue`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AdsType {
    /// The boolean type.
    Boolean,
    /// The bottom type, used for expressions that don't typecheck.
    Bottom,
    /// The integer type.
    Integer,
    /// A homogenous list type.
    List(Rc<AdsType>),
    /// The string type.
    String,
    /// A heterogenous tuple type.
    Tuple(Rc<[AdsType]>),
}

impl fmt::Display for AdsType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AdsType::Boolean => f.write_str("bool"),
            AdsType::Bottom => f.write_str("bottom"),
            AdsType::List(element) => {
                f.write_str("{")?;
                element.fmt(f)?;
                f.write_str("}")
            }
            AdsType::Integer => f.write_str("int"),
            AdsType::String => f.write_str("str"),
            AdsType::Tuple(elements) => {
                f.write_str("(")?;
                comma_separate(f, elements)?;
                f.write_str(")")
            }
        }
    }
}

//===========================================================================//

/// An expression value in an Atma Debugger Script program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AdsValue {
    /// A boolean value (false or true).
    Boolean(bool),
    /// An integer value (with no minimum/maximum range).
    Integer(BigInt),
    /// A list value.  All elements must be of the same type.
    List(Rc<[AdsValue]>),
    /// A string value.
    String(Rc<str>),
    /// A tuple value.  Its elements may be of different types.
    Tuple(Rc<[AdsValue]>),
}

impl AdsValue {
    /// Returns the contained [`Boolean`](AdsValue::Boolean) value, or panics
    /// if this value is not a boolean.
    pub fn unwrap_bool(self) -> bool {
        match self {
            AdsValue::Boolean(boolean) => boolean,
            value => panic!("AdsValue::unwrap_bool on {value:?}"),
        }
    }

    /// Returns the contained [`Integer`](AdsValue::Integer) value, or panics
    /// if this value is not an integer.
    pub fn unwrap_int(self) -> BigInt {
        match self {
            AdsValue::Integer(integer) => integer,
            value => panic!("AdsValue::unwrap_int on {value:?}"),
        }
    }

    /// Returns the contained [`List`](AdsValue::List) value, or panics if this
    /// value is not a list.
    pub fn unwrap_list(self) -> Rc<[AdsValue]> {
        match self {
            AdsValue::List(values) => values,
            value => panic!("AdsValue::unwrap_list on {value:?}"),
        }
    }

    /// Returns the contained string value, or panics if this value is not a
    /// string.
    pub fn unwrap_str(self) -> Rc<str> {
        match self {
            AdsValue::String(string) => string,
            value => panic!("AdsValue::unwrap_str on {value:?}"),
        }
    }

    /// Returns the contained [`List`](AdsValue::List) value, or panics if this
    /// value is not a list.
    pub fn unwrap_tuple(self) -> Rc<[AdsValue]> {
        match self {
            AdsValue::Tuple(values) => values,
            value => panic!("AdsValue::unwrap_tuple on {value:?}"),
        }
    }
}

impl BinaryIo for AdsValue {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            TAG_FALSE => Ok(AdsValue::Boolean(false)),
            TAG_TRUE => Ok(AdsValue::Boolean(true)),
            TAG_INTEGER => Ok(AdsValue::Integer(BigInt::read_from(reader)?)),
            TAG_LIST => {
                Ok(AdsValue::List(Rc::<[AdsValue]>::read_from(reader)?))
            }
            TAG_STRING => Ok(AdsValue::String(Rc::<str>::read_from(reader)?)),
            TAG_TUPLE => {
                Ok(AdsValue::Tuple(Rc::<[AdsValue]>::read_from(reader)?))
            }
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown value tag: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            AdsValue::Boolean(false) => TAG_FALSE.write_to(writer),
            AdsValue::Boolean(true) => TAG_TRUE.write_to(writer),
            AdsValue::Integer(integer) => {
                TAG_INTEGER.write_to(writer)?;
                integer.write_to(writer)
            }
            AdsValue::List(list) => {
                TAG_LIST.write_to(writer)?;
                list.write_to(writer)
            }
            AdsValue::String(string) => {
                TAG_STRING.write_to(writer)?;
                string.write_to(writer)
            }
            AdsValue::Tuple(tuple) => {
                TAG_TUPLE.write_to(writer)?;
                tuple.write_to(writer)
            }
        }
    }
}

impl From<bool> for AdsValue {
    fn from(value: bool) -> AdsValue {
        AdsValue::Boolean(value)
    }
}

impl fmt::Display for AdsValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AdsValue::Boolean(value) => write!(f, "%{value}"),
            AdsValue::Integer(value) => value.fmt(f),
            AdsValue::List(values) => {
                f.write_str("{")?;
                comma_separate(f, values)?;
                f.write_str("}")
            }
            AdsValue::String(value) => write!(f, "{:?}", value),
            AdsValue::Tuple(values) => {
                f.write_str("(")?;
                comma_separate(f, values)?;
                f.write_str(")")
            }
        }
    }
}

//===========================================================================//

fn comma_separate<T: fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    values: &[T],
) -> fmt::Result {
    for (i, value) in values.iter().enumerate() {
        if i != 0 {
            f.write_str(", ")?;
        }
        value.fmt(f)?;
    }
    Ok(())
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsType, AdsValue};
    use crate::obj::assert_round_trips;
    use num_bigint::BigInt;
    use std::rc::Rc;

    fn int_value(value: i32) -> AdsValue {
        AdsValue::Integer(BigInt::from(value))
    }

    fn str_value(value: &str) -> AdsValue {
        AdsValue::String(Rc::from(value))
    }

    #[test]
    fn display_basic_type() {
        assert_eq!(format!("{}", AdsType::Boolean), "bool");
        assert_eq!(format!("{}", AdsType::Integer), "int");
        assert_eq!(format!("{}", AdsType::String), "str");
    }

    #[test]
    fn display_list_type() {
        let ty = AdsType::List(Rc::from(AdsType::Integer));
        assert_eq!(format!("{}", ty), "{int}");
        let ty = AdsType::List(Rc::from(ty));
        assert_eq!(format!("{}", ty), "{{int}}");
        let ty = AdsType::List(Rc::from(AdsType::Tuple(Rc::from([
            AdsType::String,
            ty,
        ]))));
        assert_eq!(format!("{}", ty), "{(str, {{int}})}");
    }

    #[test]
    fn display_tuple_type() {
        assert_eq!(format!("{}", AdsType::Tuple(Rc::from([]))), "()");
        let ty = AdsType::Tuple(Rc::from([AdsType::Boolean, AdsType::String]));
        assert_eq!(format!("{}", ty), "(bool, str)");
    }

    #[test]
    fn display_boolean_value() {
        assert_eq!(format!("{}", AdsValue::Boolean(false)), "%false");
        assert_eq!(format!("{}", AdsValue::Boolean(true)), "%true");
    }

    #[test]
    fn display_integer_value() {
        assert_eq!(format!("{}", int_value(17)), "17");
        assert_eq!(format!("{}", int_value(0)), "0");
        assert_eq!(format!("{}", int_value(-42)), "-42");
    }

    #[test]
    fn display_string_value() {
        assert_eq!(format!("{}", str_value("")), "\"\"");
        assert_eq!(format!("{}", str_value("foo")), "\"foo\"");
        assert_eq!(format!("{}", str_value("\"")), "\"\\\"\"");
    }

    #[test]
    fn display_list_value() {
        let value = AdsValue::List(Rc::from([]));
        assert_eq!(format!("{}", value), "{}");
        let value = AdsValue::List(Rc::from([int_value(17)]));
        assert_eq!(format!("{}", value), "{17}");
        let value = AdsValue::List(Rc::from([
            int_value(4),
            int_value(-3),
            int_value(0),
        ]));
        assert_eq!(format!("{}", value), "{4, -3, 0}");
    }

    #[test]
    fn display_tuple_value() {
        let value = AdsValue::Tuple(Rc::from([]));
        assert_eq!(format!("{}", value), "()");
        let value = AdsValue::Tuple(Rc::from([
            int_value(37),
            AdsValue::Boolean(true),
        ]));
        assert_eq!(format!("{}", value), "(37, %true)");
    }

    #[test]
    fn assert_round_trips_value() {
        assert_round_trips(AdsValue::Boolean(false));
        assert_round_trips(AdsValue::Boolean(true));
        assert_round_trips(int_value(0));
        assert_round_trips(int_value(1));
        assert_round_trips(int_value(-1));
        assert_round_trips(str_value(""));
        assert_round_trips(str_value("foobar"));
    }
}

//===========================================================================//
