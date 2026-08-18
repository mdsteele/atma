use super::binary::{BinaryIo, Decoder, Encoder};
use super::context::ObjSrcContext;
use crate::error::SrcSpan;
use crate::expr::{ExprBinOp, ExprOp, ExprUnOp, ExprValue};
use num_bigint::BigInt;
use std::io;
use std::rc::Rc;

//===========================================================================//

const OP_APPLY: u8 = 0x00;
const OP_BINOP: u8 = 0x01;
const OP_GET_VALUE: u8 = 0x02;
const OP_LIST_INDEX: u8 = 0x03;
const OP_MAKE_LIST: u8 = 0x04;
const OP_MAKE_TUPLE: u8 = 0x05;
const OP_PUSH: u8 = 0x06;
const OP_SKIP: u8 = 0x07;
const OP_SKIP_IF: u8 = 0x08;
const OP_SKIP_UNLESS: u8 = 0x09;
const OP_TUPLE_ITEM: u8 = 0x0a;
const OP_UNOP: u8 = 0x0b;

//===========================================================================//

/// An expression in an assembly file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ObjExpr {
    /// The operations to perform to evaluate the expression.  Must be
    /// nonempty.
    pub(crate) ops: Vec<ObjExprOp>,
}

impl BinaryIo for ObjExpr {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let ops = Vec::<ObjExprOp>::read_from(decoder)?;
        if ops.is_empty() {
            Err(io::Error::new(io::ErrorKind::InvalidData, "empty expression"))
        } else {
            Ok(ObjExpr { ops })
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        debug_assert!(!self.ops.is_empty());
        self.ops.write_to(encoder)
    }

    fn read_option_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Option<Self>> {
        let ops = Vec::<ObjExprOp>::read_from(decoder)?;
        if ops.is_empty() { Ok(None) } else { Ok(Some(ObjExpr { ops })) }
    }

    fn write_option_to<W: io::Write>(
        option: &Option<Self>,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        match option {
            None => Vec::<ObjExprOp>::new().write_to(encoder),
            Some(value) => {
                debug_assert!(!value.ops.is_empty());
                value.ops.write_to(encoder)
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
    Apply {
        /// The source code context in which the operation appeared.
        context: Rc<ObjSrcContext>,
        /// The span of byte offsets within the context where the function
        /// expression appeared.
        func_span: SrcSpan,
        /// The span of byte offsets within the context where the function
        /// argument(s) appeared.
        arg_span: SrcSpan,
    },
    /// Pops the top two values from the value stack, evaluates the specified
    /// binary operation using the second-from-the-top value as the left-hand
    /// side and the topmost value as the right-hand side, then pushes the
    /// result onto the value stack.
    BinOp {
        /// The source code context in which the operation appeared.
        context: Rc<ObjSrcContext>,
        /// The binary operator.
        binop: ExprBinOp,
        /// The span of byte offsets within the context where the operator
        /// appeared.
        op_span: SrcSpan,
        /// The span of byte offsets within the context where the
        /// left-hand-side subexpression appeared.
        lhs_span: SrcSpan,
        /// The span of byte offsets within the context where the
        /// right-hand-side subexpression appeared.
        rhs_span: SrcSpan,
    },
    /// Copies the value at the specified index in the value stack, and pushes
    /// the copied value onto the stack.
    GetValue(usize),
    /// Pops the top two values from the value stack, and uses the topmost
    /// value (which must be an integer) as an index into the
    /// second-from-the-top value (which must be a list), then pushes that list
    /// element back onto the stack. If the index value is out of range, a
    /// link-time error will occur.
    ListIndex {
        /// The source code context in which the operation appeared.
        context: Rc<ObjSrcContext>,
        /// The span of byte offsets within the context where the list
        /// subexpression appeared.
        list_span: SrcSpan,
        /// The span of byte offsets within the context where the index
        /// subexpression appeared.
        index_span: SrcSpan,
    },
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
    /// Pops the top value from the value stack, evaluates the specified unary
    /// operation using that value, then pushes the result onto the value
    /// stack.
    UnOp {
        /// The source code context in which the operation appeared.
        context: Rc<ObjSrcContext>,
        /// The unary operator.
        unop: ExprUnOp,
        /// The span of byte offsets within the context where the operator
        /// appeared.
        op_span: SrcSpan,
        /// The span of byte offsets within the context where the argument
        /// subexpression appeared.
        arg_span: SrcSpan,
    },
}

impl BinaryIo for ObjExprOp {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            OP_APPLY => {
                let context = Rc::<ObjSrcContext>::read_from(decoder)?;
                let func_span = SrcSpan::read_from(decoder)?;
                let arg_span = SrcSpan::read_from(decoder)?;
                Ok(Self::Apply { context, func_span, arg_span })
            }
            OP_BINOP => {
                let context = Rc::<ObjSrcContext>::read_from(decoder)?;
                let binop = ExprBinOp::read_from(decoder)?;
                let op_span = SrcSpan::read_from(decoder)?;
                let lhs_span = SrcSpan::read_from(decoder)?;
                let rhs_span = SrcSpan::read_from(decoder)?;
                Ok(Self::BinOp { context, binop, op_span, lhs_span, rhs_span })
            }
            OP_GET_VALUE => Ok(Self::GetValue(usize::read_from(decoder)?)),
            OP_LIST_INDEX => {
                let context = Rc::<ObjSrcContext>::read_from(decoder)?;
                let list_span = SrcSpan::read_from(decoder)?;
                let index_span = SrcSpan::read_from(decoder)?;
                Ok(Self::ListIndex { context, list_span, index_span })
            }
            OP_MAKE_LIST => Ok(Self::MakeList(usize::read_from(decoder)?)),
            OP_MAKE_TUPLE => Ok(Self::MakeTuple(usize::read_from(decoder)?)),
            OP_PUSH => Ok(Self::Push(ExprValue::read_from(decoder)?)),
            OP_SKIP => Ok(Self::Skip(usize::read_from(decoder)?)),
            OP_SKIP_IF => Ok(Self::SkipIf(usize::read_from(decoder)?)),
            OP_SKIP_UNLESS => Ok(Self::SkipUnless(usize::read_from(decoder)?)),
            OP_TUPLE_ITEM => Ok(Self::TupleItem(usize::read_from(decoder)?)),
            OP_UNOP => {
                let context = Rc::<ObjSrcContext>::read_from(decoder)?;
                let unop = ExprUnOp::read_from(decoder)?;
                let op_span = SrcSpan::read_from(decoder)?;
                let arg_span = SrcSpan::read_from(decoder)?;
                Ok(Self::UnOp { context, unop, op_span, arg_span })
            }
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown expression opcode: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        match self {
            Self::Apply { context, func_span, arg_span } => {
                OP_APPLY.write_to(encoder)?;
                context.write_to(encoder)?;
                func_span.write_to(encoder)?;
                arg_span.write_to(encoder)
            }
            Self::BinOp { context, binop, op_span, lhs_span, rhs_span } => {
                OP_BINOP.write_to(encoder)?;
                context.write_to(encoder)?;
                binop.write_to(encoder)?;
                op_span.write_to(encoder)?;
                lhs_span.write_to(encoder)?;
                rhs_span.write_to(encoder)
            }
            Self::GetValue(index) => {
                OP_GET_VALUE.write_to(encoder)?;
                index.write_to(encoder)
            }
            Self::ListIndex { context, list_span, index_span } => {
                OP_LIST_INDEX.write_to(encoder)?;
                context.write_to(encoder)?;
                list_span.write_to(encoder)?;
                index_span.write_to(encoder)
            }
            Self::MakeList(num_items) => {
                OP_MAKE_LIST.write_to(encoder)?;
                num_items.write_to(encoder)
            }
            Self::MakeTuple(num_items) => {
                OP_MAKE_TUPLE.write_to(encoder)?;
                num_items.write_to(encoder)
            }
            Self::Push(value) => {
                OP_PUSH.write_to(encoder)?;
                value.write_to(encoder)
            }
            Self::Skip(offset) => {
                OP_SKIP.write_to(encoder)?;
                offset.write_to(encoder)
            }
            Self::SkipIf(offset) => {
                OP_SKIP_IF.write_to(encoder)?;
                offset.write_to(encoder)
            }
            Self::SkipUnless(offset) => {
                OP_SKIP_UNLESS.write_to(encoder)?;
                offset.write_to(encoder)
            }
            Self::TupleItem(index) => {
                OP_TUPLE_ITEM.write_to(encoder)?;
                index.write_to(encoder)
            }
            Self::UnOp { context, unop, op_span, arg_span } => {
                OP_UNOP.write_to(encoder)?;
                context.write_to(encoder)?;
                unop.write_to(encoder)?;
                op_span.write_to(encoder)?;
                arg_span.write_to(encoder)
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
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::ObjExprOp;
    use crate::error::SrcSpan;
    use crate::expr::{ExprBinOp, ExprUnOp, ExprValue};
    use crate::obj::{ObjSrcContext, ObjSrcParent, assert_round_trips};
    use num_bigint::BigInt;
    use std::rc::Rc;

    #[test]
    fn round_trip_obj_expr_op() {
        let context = Rc::new(ObjSrcContext {
            path: Rc::from("input"),
            parent: ObjSrcParent::Root,
        });
        assert_round_trips(ObjExprOp::Apply {
            context: context.clone(),
            func_span: SrcSpan::from_byte_range(5..10),
            arg_span: SrcSpan::from_byte_range(10..15),
        });
        assert_round_trips(ObjExprOp::BinOp {
            context: context.clone(),
            binop: ExprBinOp::BitOr,
            op_span: SrcSpan::from_byte_range(11..12),
            lhs_span: SrcSpan::from_byte_range(5..10),
            rhs_span: SrcSpan::from_byte_range(13..18),
        });
        assert_round_trips(ObjExprOp::GetValue(0));
        assert_round_trips(ObjExprOp::GetValue(42));
        assert_round_trips(ObjExprOp::ListIndex {
            context: context.clone(),
            list_span: SrcSpan::from_byte_range(5..10),
            index_span: SrcSpan::from_byte_range(11..13),
        });
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
        assert_round_trips(ObjExprOp::UnOp {
            context: context.clone(),
            unop: ExprUnOp::Neg,
            op_span: SrcSpan::from_byte_range(11..12),
            arg_span: SrcSpan::from_byte_range(13..18),
        });
    }
}

//===========================================================================//
