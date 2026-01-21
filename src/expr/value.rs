use crate::obj::BinaryIo;
use num_bigint::BigInt;
use std::cmp::Ordering;
use std::fmt;
use std::io;
use std::rc::Rc;

//===========================================================================//

const TAG_FALSE: u8 = 0;
const TAG_TRUE: u8 = 1;
const TAG_ENTITY: u8 = 2;
const TAG_INTEGER: u8 = 3;
const TAG_LIST: u8 = 4;
const TAG_STRING: u8 = 5;
const TAG_TUPLE: u8 = 6;

//===========================================================================//

/// Represents the type of an [`ExprValue`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprType {
    /// The boolean type.
    Boolean,
    /// The bottom type, used for expressions that don't typecheck.
    Bottom,
    /// An opaque object, of a kind described by the given human-readable
    /// string.
    Entity(Rc<str>),
    /// The integer type.
    Integer,
    /// A homogenous list type.
    List(Rc<ExprType>),
    /// The string type.
    String,
    /// A heterogenous tuple type.
    Tuple(Rc<[ExprType]>),
}

impl ExprType {
    /// Returns true if this type supports ordering comparisons
    /// (e.g. less-than).  If this returns true for an `ExprType`, then calling
    /// `partial_cmp` on two `ExprValue`s of that type will return a non-`None`
    /// value.
    pub(crate) fn is_ord(&self) -> bool {
        match self {
            ExprType::Boolean => true,
            ExprType::Bottom => false,
            ExprType::Entity(_) => false,
            ExprType::Integer => true,
            ExprType::List(item_type) => item_type.is_ord(),
            ExprType::String => true,
            ExprType::Tuple(item_types) => {
                item_types.iter().all(ExprType::is_ord)
            }
        }
    }
}

impl fmt::Display for ExprType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::Boolean => f.write_str("bool"),
            ExprType::Bottom => f.write_str("bottom"),
            ExprType::Entity(kind) => f.write_str(kind),
            ExprType::List(element) => {
                f.write_str("{")?;
                element.fmt(f)?;
                f.write_str("}")
            }
            ExprType::Integer => f.write_str("int"),
            ExprType::String => f.write_str("str"),
            ExprType::Tuple(elements) => {
                f.write_str("(")?;
                comma_separate(f, elements)?;
                f.write_str(")")
            }
        }
    }
}

//===========================================================================//

/// An expression value in an Atma assembly or object file or an Atma Debugger
/// Script program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprValue {
    /// A boolean value (false or true).
    Boolean(bool),
    /// An opaque object with the specified human-readable string
    /// representation.
    Entity(Rc<str>),
    /// An integer value (with no minimum/maximum range).
    Integer(BigInt),
    /// A list value.  All elements must be of the same type.
    List(Rc<[ExprValue]>),
    /// A string value.
    String(Rc<str>),
    /// A tuple value.  Its elements may be of different types.
    Tuple(Rc<[ExprValue]>),
}

impl ExprValue {
    /// Returns the contained [`Boolean`](ExprValue::Boolean) value, or panics
    /// if this value is not a boolean.
    pub fn unwrap_bool(self) -> bool {
        match self {
            ExprValue::Boolean(boolean) => boolean,
            value => panic!("ExprValue::unwrap_bool on {value:?}"),
        }
    }

    /// Returns the representation string of the contained entity value, or
    /// panics if this value is not an entity.
    pub fn unwrap_entity(self) -> Rc<str> {
        match self {
            ExprValue::Entity(repr) => repr,
            value => panic!("ExprValue::unwrap_entity on {value:?}"),
        }
    }

    /// Returns the contained [`Integer`](ExprValue::Integer) value, or panics
    /// if this value is not an integer.
    pub fn unwrap_int(self) -> BigInt {
        match self {
            ExprValue::Integer(integer) => integer,
            value => panic!("ExprValue::unwrap_int on {value:?}"),
        }
    }

    /// Returns a reference to the contained [`Integer`](ExprValue::Integer)
    /// value, or panics if this value is not an integer.
    pub fn unwrap_int_ref(&self) -> &BigInt {
        match self {
            ExprValue::Integer(integer) => integer,
            value => panic!("ExprValue::unwrap_int_ref on {value:?}"),
        }
    }

    /// Returns the contained [`List`](ExprValue::List) value, or panics if
    /// this value is not a list.
    pub fn unwrap_list(self) -> Rc<[ExprValue]> {
        match self {
            ExprValue::List(values) => values,
            value => panic!("ExprValue::unwrap_list on {value:?}"),
        }
    }

    /// Returns the contained string value, or panics if this value is not a
    /// string.
    pub fn unwrap_str(self) -> Rc<str> {
        match self {
            ExprValue::String(string) => string,
            value => panic!("ExprValue::unwrap_str on {value:?}"),
        }
    }

    /// Returns the contained [`List`](ExprValue::List) value, or panics if
    /// this value is not a list.
    pub fn unwrap_tuple(self) -> Rc<[ExprValue]> {
        match self {
            ExprValue::Tuple(values) => values,
            value => panic!("ExprValue::unwrap_tuple on {value:?}"),
        }
    }
}

impl BinaryIo for ExprValue {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            TAG_FALSE => Ok(ExprValue::Boolean(false)),
            TAG_TRUE => Ok(ExprValue::Boolean(true)),
            TAG_ENTITY => Ok(ExprValue::Entity(Rc::<str>::read_from(reader)?)),
            TAG_INTEGER => Ok(ExprValue::Integer(BigInt::read_from(reader)?)),
            TAG_LIST => {
                Ok(ExprValue::List(Rc::<[ExprValue]>::read_from(reader)?))
            }
            TAG_STRING => Ok(ExprValue::String(Rc::<str>::read_from(reader)?)),
            TAG_TUPLE => {
                Ok(ExprValue::Tuple(Rc::<[ExprValue]>::read_from(reader)?))
            }
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown value tag: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            ExprValue::Boolean(false) => TAG_FALSE.write_to(writer),
            ExprValue::Boolean(true) => TAG_TRUE.write_to(writer),
            ExprValue::Entity(repr) => {
                TAG_ENTITY.write_to(writer)?;
                repr.write_to(writer)
            }
            ExprValue::Integer(integer) => {
                TAG_INTEGER.write_to(writer)?;
                integer.write_to(writer)
            }
            ExprValue::List(list) => {
                TAG_LIST.write_to(writer)?;
                list.write_to(writer)
            }
            ExprValue::String(string) => {
                TAG_STRING.write_to(writer)?;
                string.write_to(writer)
            }
            ExprValue::Tuple(tuple) => {
                TAG_TUPLE.write_to(writer)?;
                tuple.write_to(writer)
            }
        }
    }
}

impl From<bool> for ExprValue {
    fn from(value: bool) -> ExprValue {
        ExprValue::Boolean(value)
    }
}

impl fmt::Display for ExprValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprValue::Boolean(value) => write!(f, "%{value}"),
            ExprValue::Entity(repr) => f.write_str(repr),
            ExprValue::Integer(value) => value.fmt(f),
            ExprValue::List(values) => {
                f.write_str("{")?;
                comma_separate(f, values)?;
                f.write_str("}")
            }
            ExprValue::String(value) => write!(f, "{:?}", value),
            ExprValue::Tuple(values) => {
                f.write_str("(")?;
                comma_separate(f, values)?;
                f.write_str(")")
            }
        }
    }
}

impl PartialOrd for ExprValue {
    fn partial_cmp(&self, other: &ExprValue) -> Option<Ordering> {
        match (self, other) {
            (ExprValue::Boolean(lhs), ExprValue::Boolean(rhs)) => {
                Some(lhs.cmp(rhs))
            }
            (ExprValue::Integer(lhs), ExprValue::Integer(rhs)) => {
                Some(lhs.cmp(rhs))
            }
            (ExprValue::String(lhs), ExprValue::String(rhs)) => {
                Some(lhs.cmp(rhs))
            }
            // TODO: lists
            // TODO: tuples
            _ if self == other => Some(Ordering::Equal),
            _ => None,
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
    use super::{ExprType, ExprValue};
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
    fn display_basic_type() {
        assert_eq!(format!("{}", ExprType::Boolean), "bool");
        assert_eq!(format!("{}", ExprType::Integer), "int");
        assert_eq!(format!("{}", ExprType::String), "str");
    }

    #[test]
    fn display_list_type() {
        let ty = ExprType::List(Rc::from(ExprType::Integer));
        assert_eq!(format!("{}", ty), "{int}");
        let ty = ExprType::List(Rc::from(ty));
        assert_eq!(format!("{}", ty), "{{int}}");
        let ty = ExprType::List(Rc::from(ExprType::Tuple(Rc::from([
            ExprType::String,
            ty,
        ]))));
        assert_eq!(format!("{}", ty), "{(str, {{int}})}");
    }

    #[test]
    fn display_tuple_type() {
        assert_eq!(format!("{}", ExprType::Tuple(Rc::from([]))), "()");
        let ty =
            ExprType::Tuple(Rc::from([ExprType::Boolean, ExprType::String]));
        assert_eq!(format!("{}", ty), "(bool, str)");
    }

    #[test]
    fn display_boolean_value() {
        assert_eq!(format!("{}", ExprValue::Boolean(false)), "%false");
        assert_eq!(format!("{}", ExprValue::Boolean(true)), "%true");
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
        let value = ExprValue::List(Rc::from([]));
        assert_eq!(format!("{}", value), "{}");
        let value = ExprValue::List(Rc::from([int_value(17)]));
        assert_eq!(format!("{}", value), "{17}");
        let value = ExprValue::List(Rc::from([
            int_value(4),
            int_value(-3),
            int_value(0),
        ]));
        assert_eq!(format!("{}", value), "{4, -3, 0}");
    }

    #[test]
    fn display_tuple_value() {
        let value = ExprValue::Tuple(Rc::from([]));
        assert_eq!(format!("{}", value), "()");
        let value = ExprValue::Tuple(Rc::from([
            int_value(37),
            ExprValue::Boolean(true),
        ]));
        assert_eq!(format!("{}", value), "(37, %true)");
    }

    #[test]
    fn round_trips_value() {
        assert_round_trips(ExprValue::Boolean(false));
        assert_round_trips(ExprValue::Boolean(true));
        assert_round_trips(int_value(0));
        assert_round_trips(int_value(1));
        assert_round_trips(int_value(-1));
        assert_round_trips(str_value(""));
        assert_round_trips(str_value("foobar"));
    }
}

//===========================================================================//
