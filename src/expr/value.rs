use super::func::ExprFunc;
use super::label::ExprLabel;
use crate::obj::{BinaryIo, Decoder, Encoder};
use num_bigint::BigInt;
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
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ExprType {
    /// The boolean type.
    Boolean,
    /// The bottom type, which no values inhabit, and which is a subtype of
    /// every other type.
    ///
    /// For example, this is the item type of an empty list literal.
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
    /// Used for expressions and variables that have no meaningful type because
    /// they failed to typecheck.
    ///
    /// This is also used for "wildcard" L-values (i.e. `_`), to which
    /// expressions of any type may be assigned.
    Undefined,
}

impl ExprType {
    /// Returns true if `self` is a subtype of `other` (i.e. if it is legal to
    /// assign a value of type `self` to a variable of type `other`).
    ///
    /// In general:
    /// * If the two types are the same, returns true.
    /// * If either type is `Undefined`, returns true.
    /// * If `self` is `Bottom`, returns true.
    /// * If both types are `Function`s, returns true if the output type of
    ///   `self` is a subtype of the output type of `other` AND the input type
    ///   of `other` is a subtype of the input type of `self`.
    /// * If both types are `List`s, returns true if the item type of `self` is
    ///   a subtype of the item type of `other`.
    /// * If both types are `Tuple`s of equal length, returns true if each item
    ///   type in `self` is a subtype of the corresponding item type in
    ///   `other`.
    /// * Otherwise, returns false.
    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Undefined, _) | (_, Self::Undefined) => true,
            (Self::Bottom, _) => true,
            (Self::Function(types1), Self::Function(types2)) => {
                types2.0.is_subtype_of(&types1.0)
                    && types1.1.is_subtype_of(&types2.1)
            }
            (Self::List(item1), Self::List(item2)) => {
                item1.is_subtype_of(item2)
            }
            (Self::Tuple(items1), Self::Tuple(items2)) => {
                items1.len() == items2.len()
                    && items1
                        .iter()
                        .zip(items2.iter())
                        .all(|(item1, item2)| item1.is_subtype_of(item2))
            }
            (type1, type2) => type1 == type2,
        }
    }

    /// Returns the union of two types, if one exists (e.g. the type of a
    /// ternary conditional expression whose branches have the two given
    /// types).
    ///
    /// In general:
    /// * If the two types are the same, the result is that type.
    /// * If either type is `Undefined`, the result is `Undefined`.
    /// * If either type is `Bottom`, the result is the other type.
    /// * If both types are `Function`s with the same input type, the result is
    ///   a `Function` with that input type, whose output type is the union of
    ///   the two output types (if that union exists).
    /// * If both types are `List`s, the result is a `List` of the union of the
    ///   two item types (if that union exists).
    /// * If both types are `Tuple`s of equal length, the result is a `Tuple`
    ///   of the pairwise unions of the item types (if those unions exist).
    /// * Otherwise, no union exists and the result is `None`, in which case a
    ///   type error should be reported, and `Undefined` should be used as the
    ///   unified type.
    pub fn union(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Undefined, _) | (_, Self::Undefined) => {
                Some(Self::Undefined)
            }
            (Self::Bottom, expr_type) | (expr_type, Self::Bottom) => {
                Some(expr_type.clone())
            }
            (Self::Function(types1), Self::Function(types2)) => {
                if types1.0 == types2.0 {
                    let param_type = types1.0.clone();
                    let result_type = types1.1.union(&types2.1)?;
                    Some(Self::Function(Rc::new((param_type, result_type))))
                } else {
                    None
                }
            }
            (Self::List(item1), Self::List(item2)) => {
                Some(Self::List(Rc::new(item1.union(item2)?)))
            }
            (Self::Tuple(items1), Self::Tuple(items2)) => {
                if items1.len() == items2.len() {
                    let item_types = items1
                        .iter()
                        .zip(items2.iter())
                        .map(|(item1, item2)| item1.union(item2))
                        .collect::<Option<Vec<ExprType>>>()?;
                    Some(Self::Tuple(Rc::from(item_types)))
                } else {
                    None
                }
            }
            (type1, type2) if type1 == type2 => Some(type1.clone()),
            _ => None,
        }
    }
}

impl fmt::Display for ExprType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprType::Boolean => f.write_str("bool"),
            ExprType::Bottom => f.write_str("!"),
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
            ExprType::Undefined => f.write_str("_"),
        }
    }
}

//===========================================================================//

/// An expression value in an Atma assembly or object file or an Atma Debugger
/// Script program.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_FALSE => Ok(ExprValue::Boolean(false)),
            TAG_TRUE => Ok(ExprValue::Boolean(true)),
            TAG_ENTITY => {
                Ok(ExprValue::Entity(Rc::<str>::read_from(decoder)?))
            }
            TAG_FUNCTION => {
                Ok(ExprValue::Function(ExprFunc::read_from(decoder)?))
            }
            TAG_INTEGER => Ok(ExprValue::Integer(BigInt::read_from(decoder)?)),
            TAG_LABEL_ADDR_ABS => {
                let space = Rc::<str>::read_from(decoder)?;
                let address = BigInt::read_from(decoder)?;
                Ok(ExprValue::Label(ExprLabel::AddrAbsolute {
                    space,
                    address,
                }))
            }
            TAG_LABEL_CHUNK_ABS => {
                let chunk_index = usize::read_from(decoder)?;
                let address = BigInt::read_from(decoder)?;
                Ok(ExprValue::Label(ExprLabel::ChunkAbsolute {
                    chunk_index,
                    address,
                }))
            }
            TAG_LABEL_CHUNK_REL => {
                let chunk_index = usize::read_from(decoder)?;
                let offset = BigInt::read_from(decoder)?;
                Ok(ExprValue::Label(ExprLabel::ChunkRelative {
                    chunk_index,
                    offset,
                }))
            }
            TAG_LABEL_SYMBOL_REL => {
                let name = Rc::<str>::read_from(decoder)?;
                let offset = BigInt::read_from(decoder)?;
                Ok(ExprValue::Label(ExprLabel::SymbolRelative {
                    name,
                    offset,
                }))
            }
            TAG_LIST => {
                Ok(ExprValue::List(Rc::<[ExprValue]>::read_from(decoder)?))
            }
            TAG_STRING => {
                Ok(ExprValue::String(Rc::<str>::read_from(decoder)?))
            }
            TAG_TUPLE => {
                Ok(ExprValue::Tuple(Rc::<[ExprValue]>::read_from(decoder)?))
            }
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown value tag: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        match self {
            ExprValue::Boolean(false) => TAG_FALSE.write_to(encoder),
            ExprValue::Boolean(true) => TAG_TRUE.write_to(encoder),
            ExprValue::Entity(repr) => {
                TAG_ENTITY.write_to(encoder)?;
                repr.write_to(encoder)
            }
            ExprValue::Function(func) => {
                TAG_FUNCTION.write_to(encoder)?;
                func.write_to(encoder)
            }
            ExprValue::Integer(integer) => {
                TAG_INTEGER.write_to(encoder)?;
                integer.write_to(encoder)
            }
            ExprValue::Label(ExprLabel::AddrAbsolute { space, address }) => {
                TAG_LABEL_ADDR_ABS.write_to(encoder)?;
                space.write_to(encoder)?;
                address.write_to(encoder)
            }
            ExprValue::Label(ExprLabel::ChunkAbsolute {
                chunk_index,
                address,
            }) => {
                TAG_LABEL_CHUNK_ABS.write_to(encoder)?;
                chunk_index.write_to(encoder)?;
                address.write_to(encoder)
            }
            ExprValue::Label(ExprLabel::ChunkRelative {
                chunk_index,
                offset,
            }) => {
                TAG_LABEL_CHUNK_REL.write_to(encoder)?;
                chunk_index.write_to(encoder)?;
                offset.write_to(encoder)
            }
            ExprValue::Label(ExprLabel::SymbolRelative { name, offset }) => {
                TAG_LABEL_SYMBOL_REL.write_to(encoder)?;
                name.write_to(encoder)?;
                offset.write_to(encoder)
            }
            ExprValue::List(list) => {
                TAG_LIST.write_to(encoder)?;
                list.write_to(encoder)
            }
            ExprValue::String(string) => {
                TAG_STRING.write_to(encoder)?;
                string.write_to(encoder)
            }
            ExprValue::Tuple(tuple) => {
                TAG_TUPLE.write_to(encoder)?;
                tuple.write_to(encoder)
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
        assert_eq!(ExprType::Bottom.to_string(), "!");
        assert_eq!(ExprType::Undefined.to_string(), "_");
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
        let ty = ExprType::List(Rc::from(ExprType::Bottom));
        assert_eq!(ty.to_string(), "{!}");
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
