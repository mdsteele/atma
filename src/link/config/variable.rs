use crate::obj::ObjExpr;

//===========================================================================//

/// Represents a value in a linker configuration that may be variable or
/// static.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConfigVariableOr<T> {
    /// The value will come from the linker config variable at this index.
    Variable(usize),
    /// The value is statically known.
    Static(T),
}

impl<T> ConfigVariableOr<T> {
    pub(crate) fn new_variable(
        vars: &mut Vec<ObjExpr>,
        expr: ObjExpr,
    ) -> Self {
        let index = vars.len();
        vars.push(expr);
        Self::Variable(index)
    }

    pub(crate) fn map_static<U, F: FnOnce(T) -> U>(
        self,
        func: F,
    ) -> ConfigVariableOr<U> {
        match self {
            Self::Variable(index) => ConfigVariableOr::Variable(index),
            Self::Static(value) => ConfigVariableOr::Static(func(value)),
        }
    }
}

impl<T: Default> Default for ConfigVariableOr<T> {
    fn default() -> Self {
        Self::Static(T::default())
    }
}

//===========================================================================//
