use super::binary::BinaryIo;
use crate::expr::{ExprBinOp, ExprOp, ExprValue};
use std::io;

//===========================================================================//

const OP_ADD: u8 = b'+';
const OP_LIST_INDEX: u8 = b'[';
const OP_MAKE_LIST: u8 = b'}';
const OP_MAKE_TUPLE: u8 = b')';
const OP_PUSH: u8 = b'P';
const OP_TUPLE_ITEM: u8 = b'.';

//===========================================================================//

/// An expression in an assembly file.
#[derive(Clone)]
pub struct ObjExpr {
    pub(crate) ops: Vec<ObjExprOp>,
}

impl ObjExpr {
    /// If the value of this expression is statically known at assembly time,
    /// returns that value.
    pub fn static_value(&self) -> Option<&ExprValue> {
        match self.ops.as_slice() {
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

//===========================================================================//

#[derive(Clone, Debug)]
pub(crate) enum ObjExprOp {
    Add,
    ListIndex,
    MakeList(usize),
    MakeTuple(usize),
    Push(ExprValue),
    TupleItem(usize),
}

impl BinaryIo for ObjExprOp {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            OP_ADD => Ok(ObjExprOp::Add),
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
            ObjExprOp::Add => OP_ADD.write_to(writer),
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
    fn binary_operation(_binop: ExprBinOp) -> Self {
        todo!()
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
}

//===========================================================================//
