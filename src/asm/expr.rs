use crate::expr::{
    ExprBinOp, ExprCompiler, ExprEnv, ExprOp, ExprType, ExprValue,
};
use crate::obj::BinaryIo;
use crate::parse::{ExprAst, ParseError, SrcSpan};
use std::io;

//===========================================================================//

const OP_ADD: u8 = b'+';
const OP_LIST_INDEX: u8 = b'[';
const OP_MAKE_LIST: u8 = b'}';
const OP_MAKE_TUPLE: u8 = b')';
const OP_PUSH: u8 = b'P';
const OP_TUPLE_ITEM: u8 = b'.';

//===========================================================================//

pub(crate) struct AsmTypeEnv {}

impl AsmTypeEnv {
    pub fn typecheck_expression(
        &self,
        expr: &ExprAst,
    ) -> Result<(AsmExpr, ExprType), Vec<ParseError>> {
        match ExprCompiler::new(self).typecheck(expr) {
            Ok((ops, ty)) => {
                debug_assert!(!ops.is_empty());
                Ok((AsmExpr { ops }, ty))
            }
            Err(errors) => Err(errors),
        }
    }
}

impl ExprEnv for AsmTypeEnv {
    type Op = AsmExprOp;

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        _id: &str,
    ) -> Result<(Self::Op, ExprType, Option<ExprValue>), Vec<ParseError>> {
        let message = "TODO: support identifiers".to_string();
        Err(vec![ParseError::new(span, message)])
    }
}

//===========================================================================//

/// An expression in an assembly file.
pub struct AsmExpr {
    ops: Vec<AsmExprOp>,
}

impl AsmExpr {
    /// If the value of this expression is statically known at assembly time,
    /// returns that value.
    pub fn static_value(&self) -> Option<&ExprValue> {
        match self.ops.as_slice() {
            &[AsmExprOp::Push(ref value)] => Some(value),
            _ => None,
        }
    }
}

impl BinaryIo for AsmExpr {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let ops = Vec::<AsmExprOp>::read_from(reader)?;
        if ops.is_empty() {
            Err(io::Error::new(io::ErrorKind::InvalidData, "empty expression"))
        } else {
            Ok(AsmExpr { ops })
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.ops.write_to(writer)
    }
}

//===========================================================================//

#[derive(Debug)]
pub(crate) enum AsmExprOp {
    Add,
    ListIndex,
    MakeList(usize),
    MakeTuple(usize),
    Push(ExprValue),
    TupleItem(usize),
}

impl BinaryIo for AsmExprOp {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            OP_ADD => Ok(AsmExprOp::Add),
            OP_LIST_INDEX => Ok(AsmExprOp::ListIndex),
            OP_MAKE_LIST => Ok(AsmExprOp::MakeList(usize::read_from(reader)?)),
            OP_MAKE_TUPLE => {
                Ok(AsmExprOp::MakeTuple(usize::read_from(reader)?))
            }
            OP_PUSH => Ok(AsmExprOp::Push(ExprValue::read_from(reader)?)),
            OP_TUPLE_ITEM => {
                Ok(AsmExprOp::TupleItem(usize::read_from(reader)?))
            }
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown expression opcode: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            AsmExprOp::Add => OP_ADD.write_to(writer),
            AsmExprOp::ListIndex => OP_LIST_INDEX.write_to(writer),
            AsmExprOp::MakeList(num_items) => {
                OP_MAKE_LIST.write_to(writer)?;
                num_items.write_to(writer)
            }
            AsmExprOp::MakeTuple(num_items) => {
                OP_MAKE_TUPLE.write_to(writer)?;
                num_items.write_to(writer)
            }
            AsmExprOp::Push(value) => {
                OP_PUSH.write_to(writer)?;
                value.write_to(writer)
            }
            AsmExprOp::TupleItem(index) => {
                OP_TUPLE_ITEM.write_to(writer)?;
                index.write_to(writer)
            }
        }
    }
}

impl ExprOp for AsmExprOp {
    fn binary_operation(_binop: ExprBinOp) -> Self {
        todo!()
    }

    fn list_index() -> Self {
        AsmExprOp::ListIndex
    }

    fn literal(value: ExprValue) -> Self {
        AsmExprOp::Push(value)
    }

    fn make_list(num_items: usize) -> Self {
        AsmExprOp::MakeList(num_items)
    }

    fn make_tuple(num_items: usize) -> Self {
        AsmExprOp::MakeTuple(num_items)
    }

    fn tuple_item(index: usize) -> Self {
        AsmExprOp::TupleItem(index)
    }
}

//===========================================================================//
