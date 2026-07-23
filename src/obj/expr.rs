use super::binary::BinaryIo;
use crate::expr::{ExprBinOp, ExprOp, ExprUnOp, ExprValue};
use num_bigint::BigInt;
use std::io;

//===========================================================================//

// Binary ops:
const OP_ANY_CMP_EQ: u8 = 0x00;
const OP_ANY_CMP_LE: u8 = 0x01;
const OP_ANY_CMP_LT: u8 = 0x02;
const OP_ANY_CMP_GE: u8 = 0x03;
const OP_ANY_CMP_GT: u8 = 0x04;
const OP_ANY_CMP_NE: u8 = 0x05;
const OP_BOOL_BIT_AND: u8 = 0x11;
const OP_BOOL_BIT_OR: u8 = 0x12;
const OP_BOOL_BIT_XOR: u8 = 0x13;
const OP_INT_ADD: u8 = 0x20;
const OP_INT_BIT_AND: u8 = 0x21;
const OP_INT_BIT_OR: u8 = 0x22;
const OP_INT_BIT_XOR: u8 = 0x23;
const OP_INT_DIV: u8 = 0x24;
const OP_INT_MOD: u8 = 0x25;
const OP_INT_MUL: u8 = 0x26;
const OP_INT_POW: u8 = 0x27;
const OP_INT_SHL: u8 = 0x28;
const OP_INT_SHR: u8 = 0x29;
const OP_INT_SUB: u8 = 0x2a;
const OP_LABEL_ADD_INT: u8 = 0x30;
const OP_LABEL_SUB: u8 = 0x31;
// Other ops:
const OP_APPLY: u8 = 0x40;
const OP_GET_VALUE: u8 = 0x41;
const OP_LABEL_ADDR: u8 = 0x42;
const OP_LIST_INDEX: u8 = 0x43;
const OP_MAKE_LIST: u8 = 0x44;
const OP_MAKE_TUPLE: u8 = 0x45;
const OP_PUSH: u8 = 0x46;
const OP_SKIP: u8 = 0x47;
const OP_SKIP_IF: u8 = 0x48;
const OP_SKIP_UNLESS: u8 = 0x49;
const OP_TUPLE_ITEM: u8 = 0x4a;

//===========================================================================//

/// An expression in an assembly file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjExpr {
    /// The operations to perform to evaluate the expression.  Must be
    /// nonempty.
    pub(crate) ops: Vec<ObjExprOp>,
}

impl ObjExpr {
    /// If the value of this expression is statically known at assembly time,
    /// returns that value.
    pub fn static_value(&self) -> Option<&ExprValue> {
        match &*self.ops {
            &[ObjExprOp::Push(ref value)] => Some(value),
            _ => None,
        }
    }
}

impl BinaryIo for ObjExpr {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let ops = Vec::<ObjExprOp>::read_from(reader)?;
        if ops.is_empty() {
            Err(io::Error::new(io::ErrorKind::InvalidData, "empty expression"))
        } else {
            Ok(ObjExpr { ops })
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        debug_assert!(!self.ops.is_empty());
        self.ops.write_to(writer)
    }

    fn read_option_from<R: io::BufRead>(
        reader: &mut R,
    ) -> io::Result<Option<Self>> {
        let ops = Vec::<ObjExprOp>::read_from(reader)?;
        if ops.is_empty() { Ok(None) } else { Ok(Some(ObjExpr { ops })) }
    }

    fn write_option_to<W: io::Write>(
        option: &Option<Self>,
        writer: &mut W,
    ) -> io::Result<()> {
        match option {
            None => Vec::<ObjExprOp>::new().write_to(writer),
            Some(value) => {
                debug_assert!(!value.ops.is_empty());
                value.ops.write_to(writer)
            }
        }
    }
}

impl From<ExprValue> for ObjExpr {
    fn from(value: ExprValue) -> ObjExpr {
        ObjExpr { ops: vec![ObjExprOp::Push(value)] }
    }
}

impl From<bool> for ObjExpr {
    fn from(value: bool) -> ObjExpr {
        ObjExpr::from(ExprValue::from(value))
    }
}

impl From<BigInt> for ObjExpr {
    fn from(value: BigInt) -> ObjExpr {
        ObjExpr::from(ExprValue::from(value))
    }
}

//===========================================================================//

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum ObjExprOp {
    /// Pops the top two values from the value stack, calls the
    /// second-from-the-top value with the topmost value as an argument, then
    /// pushes the result onto the value stack.
    Apply,
    /// Pops the top two values from the value stack, evaluates the specified
    /// binary operation using the second-from-the-top value as the left-hand
    /// side and the topmost value as the right-hand side, then pushes the
    /// result onto the value stack.
    BinOp(ExprBinOp),
    /// Copies the value at the specified index in the value stack, and pushes
    /// the copied value onto the stack.
    GetValue(usize),
    /// Pops the top value from the value stack (which must be a label), and
    /// pushes the runtime address of that label (as an integer) onto the value
    /// stack.
    LabelAddr,
    /// Pops the top two values from the value stack, and uses the topmost
    /// value (which must be an integer) as an index into the
    /// second-from-the-top value (which must be a list), then pushes that list
    /// element back onto the stack. If the index value is out of range, a
    /// link-time error will occur.
    ListIndex,
    /// Pops the specified number of values from the value stack (which must
    /// all have the same type), packs them into a list (with the topmost value
    /// last), then pushes that list onto the value stack.
    MakeList(usize),
    /// Pops the specified number of values from the value stack, packs them
    /// into a tuple (with the topmost value last), then pushes that tuple onto
    /// the value stack.
    MakeTuple(usize),
    /// Pushes a value onto the value stack.
    Push(ExprValue),
    /// Skips past the specified number of operations.
    Skip(usize),
    /// Pops the top value from the value stack (which must be a boolean).  If
    /// the value is true, skips past the specified number of operations.
    SkipIf(usize),
    /// Pops the top value from the value stack (which must be a boolean).  If
    /// the value is false, skips past the specified number of operations.
    SkipUnless(usize),
    /// Pops the top value from the value stack (which must be a tuple), gets
    /// the specified item from that tuple, then pushes that item onto the
    /// value stack.
    TupleItem(usize),
}

impl BinaryIo for ObjExprOp {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            // Binary ops:
            OP_ANY_CMP_EQ => Ok(Self::BinOp(ExprBinOp::AnyCmpEq)),
            OP_ANY_CMP_LE => Ok(Self::BinOp(ExprBinOp::AnyCmpLe)),
            OP_ANY_CMP_LT => Ok(Self::BinOp(ExprBinOp::AnyCmpLt)),
            OP_ANY_CMP_GE => Ok(Self::BinOp(ExprBinOp::AnyCmpGe)),
            OP_ANY_CMP_GT => Ok(Self::BinOp(ExprBinOp::AnyCmpGt)),
            OP_ANY_CMP_NE => Ok(Self::BinOp(ExprBinOp::AnyCmpNe)),
            OP_BOOL_BIT_AND => Ok(Self::BinOp(ExprBinOp::BoolBitAnd)),
            OP_BOOL_BIT_OR => Ok(Self::BinOp(ExprBinOp::BoolBitOr)),
            OP_BOOL_BIT_XOR => Ok(Self::BinOp(ExprBinOp::BoolBitXor)),
            OP_INT_ADD => Ok(Self::BinOp(ExprBinOp::IntAdd)),
            OP_INT_BIT_AND => Ok(Self::BinOp(ExprBinOp::IntBitAnd)),
            OP_INT_BIT_OR => Ok(Self::BinOp(ExprBinOp::IntBitOr)),
            OP_INT_BIT_XOR => Ok(Self::BinOp(ExprBinOp::IntBitXor)),
            OP_INT_DIV => Ok(Self::BinOp(ExprBinOp::IntDiv)),
            OP_INT_MOD => Ok(Self::BinOp(ExprBinOp::IntMod)),
            OP_INT_MUL => Ok(Self::BinOp(ExprBinOp::IntMul)),
            OP_INT_POW => Ok(Self::BinOp(ExprBinOp::IntPow)),
            OP_INT_SHL => Ok(Self::BinOp(ExprBinOp::IntShl)),
            OP_INT_SHR => Ok(Self::BinOp(ExprBinOp::IntShr)),
            OP_INT_SUB => Ok(Self::BinOp(ExprBinOp::IntSub)),
            OP_LABEL_ADD_INT => Ok(Self::BinOp(ExprBinOp::LabelAddInt)),
            OP_LABEL_SUB => Ok(Self::BinOp(ExprBinOp::LabelSub)),
            // Other ops:
            OP_APPLY => Ok(Self::Apply),
            OP_GET_VALUE => Ok(Self::GetValue(usize::read_from(reader)?)),
            OP_LABEL_ADDR => Ok(Self::LabelAddr),
            OP_LIST_INDEX => Ok(Self::ListIndex),
            OP_MAKE_LIST => Ok(Self::MakeList(usize::read_from(reader)?)),
            OP_MAKE_TUPLE => Ok(Self::MakeTuple(usize::read_from(reader)?)),
            OP_PUSH => Ok(Self::Push(ExprValue::read_from(reader)?)),
            OP_SKIP => Ok(Self::Skip(usize::read_from(reader)?)),
            OP_SKIP_IF => Ok(Self::SkipIf(usize::read_from(reader)?)),
            OP_SKIP_UNLESS => Ok(Self::SkipUnless(usize::read_from(reader)?)),
            OP_TUPLE_ITEM => Ok(Self::TupleItem(usize::read_from(reader)?)),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown expression opcode: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            Self::Apply => OP_APPLY.write_to(writer),
            Self::BinOp(ExprBinOp::AnyCmpEq) => OP_ANY_CMP_EQ.write_to(writer),
            Self::BinOp(ExprBinOp::AnyCmpLe) => OP_ANY_CMP_LE.write_to(writer),
            Self::BinOp(ExprBinOp::AnyCmpLt) => OP_ANY_CMP_LT.write_to(writer),
            Self::BinOp(ExprBinOp::AnyCmpGe) => OP_ANY_CMP_GE.write_to(writer),
            Self::BinOp(ExprBinOp::AnyCmpGt) => OP_ANY_CMP_GT.write_to(writer),
            Self::BinOp(ExprBinOp::AnyCmpNe) => OP_ANY_CMP_NE.write_to(writer),
            Self::BinOp(ExprBinOp::BoolBitAnd) => {
                OP_BOOL_BIT_AND.write_to(writer)
            }
            Self::BinOp(ExprBinOp::BoolBitOr) => {
                OP_BOOL_BIT_OR.write_to(writer)
            }
            Self::BinOp(ExprBinOp::BoolBitXor) => {
                OP_BOOL_BIT_XOR.write_to(writer)
            }
            Self::BinOp(ExprBinOp::IntAdd) => OP_INT_ADD.write_to(writer),
            Self::BinOp(ExprBinOp::IntBitAnd) => {
                OP_INT_BIT_AND.write_to(writer)
            }
            Self::BinOp(ExprBinOp::IntBitOr) => OP_INT_BIT_OR.write_to(writer),
            Self::BinOp(ExprBinOp::IntBitXor) => {
                OP_INT_BIT_XOR.write_to(writer)
            }
            Self::BinOp(ExprBinOp::IntDiv) => OP_INT_DIV.write_to(writer),
            Self::BinOp(ExprBinOp::IntMod) => OP_INT_MOD.write_to(writer),
            Self::BinOp(ExprBinOp::IntMul) => OP_INT_MUL.write_to(writer),
            Self::BinOp(ExprBinOp::IntPow) => OP_INT_POW.write_to(writer),
            Self::BinOp(ExprBinOp::IntShl) => OP_INT_SHL.write_to(writer),
            Self::BinOp(ExprBinOp::IntShr) => OP_INT_SHR.write_to(writer),
            Self::BinOp(ExprBinOp::IntSub) => OP_INT_SUB.write_to(writer),
            Self::BinOp(ExprBinOp::LabelAddInt) => {
                OP_LABEL_ADD_INT.write_to(writer)
            }
            Self::BinOp(ExprBinOp::LabelSub) => OP_LABEL_SUB.write_to(writer),
            Self::GetValue(index) => {
                OP_GET_VALUE.write_to(writer)?;
                index.write_to(writer)
            }
            Self::LabelAddr => OP_LABEL_ADDR.write_to(writer),
            Self::ListIndex => OP_LIST_INDEX.write_to(writer),
            Self::MakeList(num_items) => {
                OP_MAKE_LIST.write_to(writer)?;
                num_items.write_to(writer)
            }
            Self::MakeTuple(num_items) => {
                OP_MAKE_TUPLE.write_to(writer)?;
                num_items.write_to(writer)
            }
            Self::Push(value) => {
                OP_PUSH.write_to(writer)?;
                value.write_to(writer)
            }
            Self::Skip(offset) => {
                OP_SKIP.write_to(writer)?;
                offset.write_to(writer)
            }
            Self::SkipIf(offset) => {
                OP_SKIP_IF.write_to(writer)?;
                offset.write_to(writer)
            }
            Self::SkipUnless(offset) => {
                OP_SKIP_UNLESS.write_to(writer)?;
                offset.write_to(writer)
            }
            Self::TupleItem(index) => {
                OP_TUPLE_ITEM.write_to(writer)?;
                index.write_to(writer)
            }
        }
    }
}

impl ExprOp for ObjExprOp {
    fn literal(value: ExprValue) -> Self {
        Self::Push(value)
    }

    fn make_list(num_items: usize) -> Self {
        Self::MakeList(num_items)
    }

    fn make_tuple(num_items: usize) -> Self {
        Self::MakeTuple(num_items)
    }

    fn skip(offset: usize) -> Self {
        Self::Skip(offset)
    }

    fn skip_if(offset: usize) -> Self {
        Self::SkipIf(offset)
    }

    fn skip_unless(offset: usize) -> Self {
        Self::SkipUnless(offset)
    }

    fn tuple_item(index: usize) -> Self {
        Self::TupleItem(index)
    }

    fn unary_operation(_unop: ExprUnOp) -> Self {
        todo!()
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::ObjExprOp;
    use crate::expr::{ExprBinOp, ExprValue};
    use crate::obj::assert_round_trips;
    use num_bigint::BigInt;

    #[test]
    fn round_trip_obj_expr_op() {
        assert_round_trips(ObjExprOp::Apply);
        assert_round_trips(ObjExprOp::BinOp(ExprBinOp::IntAdd));
        assert_round_trips(ObjExprOp::BinOp(ExprBinOp::LabelSub));
        assert_round_trips(ObjExprOp::GetValue(0));
        assert_round_trips(ObjExprOp::GetValue(42));
        assert_round_trips(ObjExprOp::LabelAddr);
        assert_round_trips(ObjExprOp::ListIndex);
        assert_round_trips(ObjExprOp::MakeList(0));
        assert_round_trips(ObjExprOp::MakeList(3));
        assert_round_trips(ObjExprOp::MakeTuple(0));
        assert_round_trips(ObjExprOp::MakeTuple(2));
        assert_round_trips(ObjExprOp::Push(ExprValue::Boolean(false)));
        assert_round_trips(ObjExprOp::Push(ExprValue::Integer(BigInt::from(
            12345u32,
        ))));
        assert_round_trips(ObjExprOp::Skip(1));
        assert_round_trips(ObjExprOp::Skip(17));
        assert_round_trips(ObjExprOp::SkipIf(2));
        assert_round_trips(ObjExprOp::SkipIf(13));
        assert_round_trips(ObjExprOp::SkipUnless(2));
        assert_round_trips(ObjExprOp::SkipUnless(1234));
        assert_round_trips(ObjExprOp::TupleItem(0));
        assert_round_trips(ObjExprOp::TupleItem(2));
    }
}

//===========================================================================//
