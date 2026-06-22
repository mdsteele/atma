use super::func::ExprFunc;
use super::label::ExprLabel;
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
const TAG_FUNCTION: u8 = 3;
const TAG_INTEGER: u8 = 4;
const TAG_LABEL_ADDR_ABS: u8 = 5;
const TAG_LABEL_CHUNK_ABS: u8 = 6;
const TAG_LABEL_CHUNK_REL: u8 = 7;
const TAG_LABEL_SYMBOL_REL: u8 = 8;
const TAG_LIST: u8 = 9;
const TAG_STRING: u8 = 10;
const TAG_TUPLE: u8 = 11;

//===========================================================================//

/// Represents the type of an [`ExprValue`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprType {
    /// The boolean type.
    Boolean,
    /// The bottom type, used for expressions that don't typecheck.
    Bottom,
    /// An opaque object type with the given human-readable type name.
    Entity(Rc<str>),
    /// A function type, with the given input and output types.
    Function(Rc<(ExprType, ExprType)>),
    /// The (unlimited-precision) integer type.
    Integer,
    /// The type comprising memory locations within an assembly file, compiled
    /// binary, or runtime address space.
    Label,
    /// A homogenous list type, with elements of the given type.
    List(Rc<ExprType>),
    /// The string type.
    String,
    /// A heterogenous tuple type, with elements of the given types.
    Tuple(Rc<[ExprType]>),
}

impl ExprType {
    /// Returns true if this type supports totally ordered comparisons.  If
    /// this returns true for an `ExprType`, then calling `partial_cmp` on two
    /// `ExprValue`s of that type will always return a non-`None` value.
    pub(crate) fn is_ord(&self) -> bool {
        match self {
            ExprType::Boolean => true,
            ExprType::Bottom => false,
            ExprType::Entity(_) => false,
            ExprType::Function(_) => false,
            ExprType::Integer => true,
            ExprType::Label => false,
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
            ExprType::Entity(type_name) => f.write_str(type_name),
            ExprType::Function(types) => {
                f.write_str("[")?;
                types.0.fmt(f)?;
                f.write_str(" -> ")?;
                types.1.fmt(f)?;
                f.write_str("]")
            }
            ExprType::Label => f.write_str("label"),
            ExprType::List(item_type) => {
                f.write_str("{")?;
                item_type.fmt(f)?;
                f.write_str("}")
            }
            ExprType::Integer => f.write_str("int"),
            ExprType::String => f.write_str("str"),
            ExprType::Tuple(item_types) => {
                f.write_str("(")?;
                comma_separate(f, item_types)?;
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
    /// A function.
    Function(ExprFunc),
    /// An integer value (with no minimum/maximum range).
    Integer(BigInt),
    /// A memory location.
    Label(ExprLabel),
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
    pub fn unwrap_bool(&self) -> bool {
        match self {
            &ExprValue::Boolean(boolean) => boolean,
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

    /// Returns the contained [`Function`](ExprValue::Function) value, or
    /// panics if this value is not a function.
    pub fn unwrap_func(self) -> ExprFunc {
        match self {
            ExprValue::Function(func) => func,
            value => panic!("ExprValue::unwrap_func on {value:?}"),
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

    /// Returns the contained [`Label`](ExprValue::Label) value, or panics if
    /// this value is not a label.
    pub fn unwrap_label(self) -> ExprLabel {
        match self {
            ExprValue::Label(label) => label,
            value => panic!("ExprValue::unwrap_label on {value:?}"),
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

    /// Returns a referenced to the contained string value, or panics if this
    /// value is not a string.
    pub fn unwrap_str_ref(&self) -> &Rc<str> {
        match self {
            ExprValue::String(string) => string,
            value => panic!("ExprValue::unwrap_str_ref on {value:?}"),
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
            TAG_FUNCTION => {
                Ok(ExprValue::Function(ExprFunc::read_from(reader)?))
            }
            TAG_INTEGER => Ok(ExprValue::Integer(BigInt::read_from(reader)?)),
            TAG_LABEL_ADDR_ABS => {
                let space = Rc::<str>::read_from(reader)?;
                let address = BigInt::read_from(reader)?;
                Ok(ExprValue::Label(ExprLabel::AddrAbsolute {
                    space,
                    address,
                }))
            }
            TAG_LABEL_CHUNK_ABS => {
                let chunk_index = usize::read_from(reader)?;
                let address = BigInt::read_from(reader)?;
                Ok(ExprValue::Label(ExprLabel::ChunkAbsolute {
                    chunk_index,
                    address,
                }))
            }
            TAG_LABEL_CHUNK_REL => {
                let chunk_index = usize::read_from(reader)?;
                let offset = BigInt::read_from(reader)?;
                Ok(ExprValue::Label(ExprLabel::ChunkRelative {
                    chunk_index,
                    offset,
                }))
            }
            TAG_LABEL_SYMBOL_REL => {
                let name = Rc::<str>::read_from(reader)?;
                let offset = BigInt::read_from(reader)?;
                Ok(ExprValue::Label(ExprLabel::SymbolRelative {
                    name,
                    offset,
                }))
            }
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
            ExprValue::Function(func) => {
                TAG_FUNCTION.write_to(writer)?;
                func.write_to(writer)
            }
            ExprValue::Integer(integer) => {
                TAG_INTEGER.write_to(writer)?;
                integer.write_to(writer)
            }
            ExprValue::Label(ExprLabel::AddrAbsolute { space, address }) => {
                TAG_LABEL_ADDR_ABS.write_to(writer)?;
                space.write_to(writer)?;
                address.write_to(writer)
            }
            ExprValue::Label(ExprLabel::ChunkAbsolute {
                chunk_index,
                address,
            }) => {
                TAG_LABEL_CHUNK_ABS.write_to(writer)?;
                chunk_index.write_to(writer)?;
                address.write_to(writer)
            }
            ExprValue::Label(ExprLabel::ChunkRelative {
                chunk_index,
                offset,
            }) => {
                TAG_LABEL_CHUNK_REL.write_to(writer)?;
                chunk_index.write_to(writer)?;
                offset.write_to(writer)
            }
            ExprValue::Label(ExprLabel::SymbolRelative { name, offset }) => {
                TAG_LABEL_SYMBOL_REL.write_to(writer)?;
                name.write_to(writer)?;
                offset.write_to(writer)
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

impl From<BigInt> for ExprValue {
    fn from(value: BigInt) -> ExprValue {
        ExprValue::Integer(value)
    }
}

impl fmt::Display for ExprValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprValue::Boolean(value) => write!(f, "%{value}"),
            ExprValue::Entity(repr) => f.write_str(repr),
            ExprValue::Function(func) => func.fmt(f),
            ExprValue::Integer(value) => value.fmt(f),
            ExprValue::Label(label) => label.fmt(f),
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
            (ExprValue::Label(lhs), ExprValue::Label(rhs)) => {
                lhs.partial_cmp(rhs)
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
    use super::{ExprLabel, ExprType, ExprValue};
    use crate::obj::assert_round_trips;
    use num_bigint::BigInt;
    use std::rc::Rc;

    fn func_type(input: ExprType, output: ExprType) -> ExprType {
        ExprType::Function(Rc::from((input, output)))
    }

    fn int_value(value: i32) -> ExprValue {
        ExprValue::Integer(BigInt::from(value))
    }

    fn str_value(value: &str) -> ExprValue {
        ExprValue::String(Rc::from(value))
    }

    #[test]
    fn display_basic_type() {
        assert_eq!(ExprType::Boolean.to_string(), "bool");
        assert_eq!(ExprType::Integer.to_string(), "int");
        assert_eq!(ExprType::Label.to_string(), "label");
        assert_eq!(ExprType::String.to_string(), "str");
    }

    #[test]
    fn display_function_type() {
        assert_eq!(
            func_type(ExprType::Integer, ExprType::Boolean).to_string(),
            "[int -> bool]"
        );
        assert_eq!(
            func_type(
                ExprType::Integer,
                func_type(ExprType::Boolean, ExprType::String)
            )
            .to_string(),
            "[int -> [bool -> str]]"
        );
        assert_eq!(
            func_type(
                func_type(ExprType::Integer, ExprType::Boolean),
                ExprType::String
            )
            .to_string(),
            "[[int -> bool] -> str]"
        );
        assert_eq!(
            func_type(
                ExprType::Tuple(Rc::from([
                    ExprType::String,
                    ExprType::Integer
                ])),
                ExprType::List(Rc::from(ExprType::String))
            )
            .to_string(),
            "[(str, int) -> {str}]"
        );
    }

    #[test]
    fn display_list_type() {
        let ty = ExprType::List(Rc::from(ExprType::Integer));
        assert_eq!(ty.to_string(), "{int}");
        let ty = ExprType::List(Rc::from(ty));
        assert_eq!(ty.to_string(), "{{int}}");
        let ty = ExprType::List(Rc::from(ExprType::Tuple(Rc::from([
            ExprType::String,
            ty,
        ]))));
        assert_eq!(ty.to_string(), "{(str, {{int}})}");
    }

    #[test]
    fn display_tuple_type() {
        assert_eq!(ExprType::Tuple(Rc::from([])).to_string(), "()");
        let ty =
            ExprType::Tuple(Rc::from([ExprType::Boolean, ExprType::String]));
        assert_eq!(ty.to_string(), "(bool, str)");
    }

    #[test]
    fn display_boolean_value() {
        assert_eq!(ExprValue::Boolean(false).to_string(), "%false");
        assert_eq!(ExprValue::Boolean(true).to_string(), "%true");
    }

    #[test]
    fn display_integer_value() {
        assert_eq!(int_value(17).to_string(), "17");
        assert_eq!(int_value(0).to_string(), "0");
        assert_eq!(int_value(-42).to_string(), "-42");
    }

    #[test]
    fn display_label_value() {
        let value = ExprValue::Label(ExprLabel::SymbolRelative {
            name: Rc::from("Foo"),
            offset: BigInt::from(0x20u32),
        });
        assert_eq!(value.to_string(), "Foo + $20");
    }

    #[test]
    fn display_string_value() {
        assert_eq!(str_value("").to_string(), "\"\"");
        assert_eq!(str_value("foo").to_string(), "\"foo\"");
        assert_eq!(str_value("\"").to_string(), "\"\\\"\"");
    }

    #[test]
    fn display_list_value() {
        let value = ExprValue::List(Rc::from([]));
        assert_eq!(value.to_string(), "{}");
        let value = ExprValue::List(Rc::from([int_value(17)]));
        assert_eq!(value.to_string(), "{17}");
        let value = ExprValue::List(Rc::from([
            int_value(4),
            int_value(-3),
            int_value(0),
        ]));
        assert_eq!(value.to_string(), "{4, -3, 0}");
    }

    #[test]
    fn display_tuple_value() {
        let value = ExprValue::Tuple(Rc::from([]));
        assert_eq!(value.to_string(), "()");
        let value = ExprValue::Tuple(Rc::from([
            int_value(37),
            ExprValue::Boolean(true),
        ]));
        assert_eq!(value.to_string(), "(37, %true)");
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
