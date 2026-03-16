use super::binary::BinaryIo;
use crate::expr::{ExprBinOp, ExprOp, ExprUnOp, ExprValue};
use std::io;

//===========================================================================//

// Binary ops:
const OP_ANY_CMP_EQ: u8 = 0x00;
const OP_ANY_CMP_LE: u8 = 0x01;
const OP_ANY_CMP_LT: u8 = 0x02;
const OP_ANY_CMP_GE: u8 = 0x03;
const OP_ANY_CMP_GT: u8 = 0x04;
const OP_ANY_CMP_NE: u8 = 0x05;
const OP_INT_ADD: u8 = 0x10;
const OP_INT_BIT_AND: u8 = 0x11;
const OP_INT_BIT_OR: u8 = 0x12;
const OP_INT_BIT_XOR: u8 = 0x13;
const OP_INT_DIV: u8 = 0x14;
const OP_INT_MOD: u8 = 0x15;
const OP_INT_MUL: u8 = 0x16;
const OP_INT_POW: u8 = 0x17;
const OP_INT_SHL: u8 = 0x18;
const OP_INT_SHR: u8 = 0x19;
const OP_INT_SUB: u8 = 0x1a;
const OP_LABEL_ADD_INT: u8 = 0x20;
const OP_LABEL_SUB: u8 = 0x21;
// Other ops:
const OP_GET_VALUE: u8 = 0x30;
const OP_LABEL_ADDR: u8 = 0x31;
const OP_LIST_INDEX: u8 = 0x32;
const OP_MAKE_LIST: u8 = 0x33;
const OP_MAKE_TUPLE: u8 = 0x34;
const OP_PUSH: u8 = 0x35;
const OP_TUPLE_ITEM: u8 = 0x36;

//===========================================================================//

/// An expression in an assembly file.
pub struct ObjExpr {
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
        self.ops.write_to(writer)
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

//===========================================================================//

#[derive(Clone, Debug)]
pub(crate) enum ObjExprOp {
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
    /// Pops the top value from the value stack (which must be a tuple), gets
    /// the specified item from that tuple, then pushes that item onto the
    /// value stack.
    TupleItem(usize),
}

impl BinaryIo for ObjExprOp {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            // Binary ops:
            OP_ANY_CMP_EQ => Ok(ObjExprOp::BinOp(ExprBinOp::AnyCmpEq)),
            OP_ANY_CMP_LE => Ok(ObjExprOp::BinOp(ExprBinOp::AnyCmpLe)),
            OP_ANY_CMP_LT => Ok(ObjExprOp::BinOp(ExprBinOp::AnyCmpLt)),
            OP_ANY_CMP_GE => Ok(ObjExprOp::BinOp(ExprBinOp::AnyCmpGe)),
            OP_ANY_CMP_GT => Ok(ObjExprOp::BinOp(ExprBinOp::AnyCmpGt)),
            OP_ANY_CMP_NE => Ok(ObjExprOp::BinOp(ExprBinOp::AnyCmpNe)),
            OP_INT_ADD => Ok(ObjExprOp::BinOp(ExprBinOp::IntAdd)),
            OP_INT_BIT_AND => Ok(ObjExprOp::BinOp(ExprBinOp::IntBitAnd)),
            OP_INT_BIT_OR => Ok(ObjExprOp::BinOp(ExprBinOp::IntBitOr)),
            OP_INT_BIT_XOR => Ok(ObjExprOp::BinOp(ExprBinOp::IntBitXor)),
            OP_INT_DIV => Ok(ObjExprOp::BinOp(ExprBinOp::IntDiv)),
            OP_INT_MOD => Ok(ObjExprOp::BinOp(ExprBinOp::IntMod)),
            OP_INT_MUL => Ok(ObjExprOp::BinOp(ExprBinOp::IntMul)),
            OP_INT_POW => Ok(ObjExprOp::BinOp(ExprBinOp::IntPow)),
            OP_INT_SHL => Ok(ObjExprOp::BinOp(ExprBinOp::IntShl)),
            OP_INT_SHR => Ok(ObjExprOp::BinOp(ExprBinOp::IntShr)),
            OP_INT_SUB => Ok(ObjExprOp::BinOp(ExprBinOp::IntSub)),
            OP_LABEL_ADD_INT => Ok(ObjExprOp::BinOp(ExprBinOp::LabelAddInt)),
            OP_LABEL_SUB => Ok(ObjExprOp::BinOp(ExprBinOp::LabelSub)),
            // Other ops:
            OP_GET_VALUE => Ok(ObjExprOp::GetValue(usize::read_from(reader)?)),
            OP_LABEL_ADDR => Ok(ObjExprOp::LabelAddr),
            OP_LIST_INDEX => Ok(ObjExprOp::ListIndex),
            OP_MAKE_LIST => Ok(ObjExprOp::MakeList(usize::read_from(reader)?)),
            OP_MAKE_TUPLE => {
                Ok(ObjExprOp::MakeTuple(usize::read_from(reader)?))
            }
            OP_PUSH => Ok(ObjExprOp::Push(ExprValue::read_from(reader)?)),
            OP_TUPLE_ITEM => {
                Ok(ObjExprOp::TupleItem(usize::read_from(reader)?))
            }
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown expression opcode: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            ObjExprOp::BinOp(ExprBinOp::AnyCmpEq) => {
                OP_ANY_CMP_EQ.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::AnyCmpLe) => {
                OP_ANY_CMP_LE.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::AnyCmpLt) => {
                OP_ANY_CMP_LT.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::AnyCmpGe) => {
                OP_ANY_CMP_GE.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::AnyCmpGt) => {
                OP_ANY_CMP_GT.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::AnyCmpNe) => {
                OP_ANY_CMP_NE.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::IntAdd) => OP_INT_ADD.write_to(writer),
            ObjExprOp::BinOp(ExprBinOp::IntBitAnd) => {
                OP_INT_BIT_AND.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::IntBitOr) => {
                OP_INT_BIT_OR.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::IntBitXor) => {
                OP_INT_BIT_XOR.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::IntDiv) => OP_INT_DIV.write_to(writer),
            ObjExprOp::BinOp(ExprBinOp::IntMod) => OP_INT_MOD.write_to(writer),
            ObjExprOp::BinOp(ExprBinOp::IntMul) => OP_INT_MUL.write_to(writer),
            ObjExprOp::BinOp(ExprBinOp::IntPow) => OP_INT_POW.write_to(writer),
            ObjExprOp::BinOp(ExprBinOp::IntShl) => OP_INT_SHL.write_to(writer),
            ObjExprOp::BinOp(ExprBinOp::IntShr) => OP_INT_SHR.write_to(writer),
            ObjExprOp::BinOp(ExprBinOp::IntSub) => OP_INT_SUB.write_to(writer),
            ObjExprOp::BinOp(ExprBinOp::LabelAddInt) => {
                OP_LABEL_ADD_INT.write_to(writer)
            }
            ObjExprOp::BinOp(ExprBinOp::LabelSub) => {
                OP_LABEL_SUB.write_to(writer)
            }
            ObjExprOp::GetValue(index) => {
                OP_GET_VALUE.write_to(writer)?;
                index.write_to(writer)
            }
            ObjExprOp::LabelAddr => OP_LABEL_ADDR.write_to(writer),
            ObjExprOp::ListIndex => OP_LIST_INDEX.write_to(writer),
            ObjExprOp::MakeList(num_items) => {
                OP_MAKE_LIST.write_to(writer)?;
                num_items.write_to(writer)
            }
            ObjExprOp::MakeTuple(num_items) => {
                OP_MAKE_TUPLE.write_to(writer)?;
                num_items.write_to(writer)
            }
            ObjExprOp::Push(value) => {
                OP_PUSH.write_to(writer)?;
                value.write_to(writer)
            }
            ObjExprOp::TupleItem(index) => {
                OP_TUPLE_ITEM.write_to(writer)?;
                index.write_to(writer)
            }
        }
    }
}

impl ExprOp for ObjExprOp {
    fn binary_operation(binop: ExprBinOp) -> Self {
        ObjExprOp::BinOp(binop)
    }

    fn list_index() -> Self {
        ObjExprOp::ListIndex
    }

    fn literal(value: ExprValue) -> Self {
        ObjExprOp::Push(value)
    }

    fn make_list(num_items: usize) -> Self {
        ObjExprOp::MakeList(num_items)
    }

    fn make_tuple(num_items: usize) -> Self {
        ObjExprOp::MakeTuple(num_items)
    }

    fn tuple_item(index: usize) -> Self {
        ObjExprOp::TupleItem(index)
    }

    fn unary_operation(_unop: ExprUnOp) -> Self {
        todo!()
    }
}

//===========================================================================//
