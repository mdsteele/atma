use num_bigint::BigInt;
use std::fmt;
use std::rc::Rc;

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
    Tuple(Rc<Vec<AdsType>>),
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
    List(Vec<AdsValue>),
    /// A string value.
    String(String),
    /// A tuple value.  Its elements may be of different types.
    Tuple(Vec<AdsValue>),
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
}

impl From<bool> for AdsValue {
    fn from(value: bool) -> AdsValue {
        AdsValue::Boolean(value)
    }
}

impl fmt::Display for AdsValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AdsValue::Boolean(value) => value.fmt(f),
            AdsValue::Integer(value) => value.fmt(f),
            AdsValue::List(values) => {
                f.write_str("{")?;
                comma_separate(f, values)?;
                f.write_str("}")
            }
            AdsValue::String(value) => value.fmt(f),
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
    use num_bigint::BigInt;
    use std::rc::Rc;

    fn int_value(value: i32) -> AdsValue {
        AdsValue::Integer(BigInt::from(value))
    }

    fn str_value(value: &str) -> AdsValue {
        AdsValue::String(value.to_string())
    }

    #[test]
    fn display_basic_type() {
        assert_eq!(format!("{}", AdsType::Boolean), "bool");
        assert_eq!(format!("{}", AdsType::Integer), "int");
        assert_eq!(format!("{}", AdsType::String), "str");
    }

    #[test]
    fn display_list_type() {
        let ty = AdsType::List(Rc::new(AdsType::Integer));
        assert_eq!(format!("{}", ty), "{int}");
        let ty = AdsType::List(Rc::new(ty));
        assert_eq!(format!("{}", ty), "{{int}}");
        let ty = AdsType::List(Rc::new(AdsType::Tuple(Rc::new(vec![
            AdsType::String,
            ty,
        ]))));
        assert_eq!(format!("{}", ty), "{(str, {{int}})}");
    }

    #[test]
    fn display_tuple_type() {
        assert_eq!(format!("{}", AdsType::Tuple(Rc::new(vec![]))), "()");
        let ty =
            AdsType::Tuple(Rc::new(vec![AdsType::Boolean, AdsType::String]));
        assert_eq!(format!("{}", ty), "(bool, str)");
    }

    #[test]
    fn display_boolean_value() {
        assert_eq!(format!("{}", AdsValue::Boolean(false)), "false");
        assert_eq!(format!("{}", AdsValue::Boolean(true)), "true");
    }

    #[test]
    fn display_integer_value() {
        assert_eq!(format!("{}", int_value(17)), "17");
        assert_eq!(format!("{}", int_value(0)), "0");
        assert_eq!(format!("{}", int_value(-42)), "-42");
    }

    #[test]
    fn display_string_value() {
        assert_eq!(format!("{}", str_value("")), "");
        assert_eq!(format!("{}", str_value("foo")), "foo");
    }

    #[test]
    fn display_list_value() {
        let value = AdsValue::List(vec![]);
        assert_eq!(format!("{}", value), "{}");
        let value = AdsValue::List(vec![int_value(17)]);
        assert_eq!(format!("{}", value), "{17}");
        let value =
            AdsValue::List(vec![int_value(4), int_value(-3), int_value(0)]);
        assert_eq!(format!("{}", value), "{4, -3, 0}");
    }

    #[test]
    fn display_tuple_value() {
        let value = AdsValue::Tuple(vec![]);
        assert_eq!(format!("{}", value), "()");
        let value =
            AdsValue::Tuple(vec![int_value(37), AdsValue::Boolean(true)]);
        assert_eq!(format!("{}", value), "(37, true)");
    }
}

//===========================================================================//
