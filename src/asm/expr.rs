use crate::db::{AdsType, AdsValue};
use crate::obj::BinaryIo;
use crate::parse::{ExprAst, ExprAstNode, ParseError};
use std::io;
use std::rc::Rc;

//===========================================================================//

const OP_ADD: u8 = b'+';
const OP_PUSH: u8 = b'P';

//===========================================================================//

pub struct AsmTypeEnv {}

impl AsmTypeEnv {
    pub fn typecheck_expression(
        &self,
        expr: &ExprAst,
    ) -> Result<(AsmExpr, AdsType), Vec<ParseError>> {
        AsmExprCompiler::new(self).typecheck(expr)
    }
}

//===========================================================================//

struct AsmExprCompiler<'a> {
    _env: &'a AsmTypeEnv,
    // Invariant: If the top N entries of `types` all hold `is_static = true`,
    // then the top N entries of `ops` are all safe for
    // `AsmExprCompiler::unwrap_static()`.
    types: Vec<(AdsType, bool)>, // (type, is_static)
    ops: Vec<AsmExprOp>,
    errors: Vec<ParseError>,
}

impl<'a> AsmExprCompiler<'a> {
    pub fn new(env: &'a AsmTypeEnv) -> AsmExprCompiler<'a> {
        AsmExprCompiler {
            _env: env,
            types: Vec::new(),
            ops: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn typecheck(
        mut self,
        expr: &ExprAst,
    ) -> Result<(AsmExpr, AdsType), Vec<ParseError>> {
        let mut stack = vec![expr];
        let mut subexprs = Vec::<&ExprAst>::new();
        while let Some(subexpr) = stack.pop() {
            subexprs.push(subexpr);
            match &subexpr.node {
                ExprAstNode::BinOp(_, lhs, rhs) => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                ExprAstNode::BoolLiteral(_) => {}
                ExprAstNode::Identifier(_) => {}
                ExprAstNode::Index(_, lhs, rhs) => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                ExprAstNode::IntLiteral(_) => {}
                ExprAstNode::ListLiteral(items) => stack.extend(items),
                ExprAstNode::StrLiteral(_) => {}
                ExprAstNode::TupleLiteral(items) => stack.extend(items),
            }
        }
        for subexpr in subexprs.into_iter().rev() {
            self.typecheck_subexpr(subexpr);
        }
        debug_assert_eq!(self.types.len(), 1);
        if self.errors.is_empty() {
            debug_assert!(!self.ops.is_empty());
            Ok((AsmExpr { ops: self.ops }, self.types.pop().unwrap().0))
        } else {
            Err(self.errors)
        }
    }

    fn typecheck_subexpr(&mut self, subexpr: &ExprAst) {
        match &subexpr.node {
            ExprAstNode::BoolLiteral(value) => {
                self.ops.push(AsmExprOp::Push(AdsValue::Boolean(*value)));
                self.types.push((AdsType::Boolean, true));
            }
            ExprAstNode::IntLiteral(value) => {
                self.ops
                    .push(AsmExprOp::Push(AdsValue::Integer(value.clone())));
                self.types.push((AdsType::Integer, true));
            }
            ExprAstNode::StrLiteral(value) => {
                self.ops.push(AsmExprOp::Push(AdsValue::String(Rc::from(
                    value.as_str(),
                ))));
                self.types.push((AdsType::String, true));
            }
            _ => todo!(),
        }
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
    pub fn static_value(&self) -> Option<&AdsValue> {
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

enum AsmExprOp {
    Add,
    Push(AdsValue),
}

impl BinaryIo for AsmExprOp {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            OP_ADD => Ok(AsmExprOp::Add),
            OP_PUSH => Ok(AsmExprOp::Push(AdsValue::read_from(reader)?)),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown expression opcode: 0x{:02x}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        match self {
            AsmExprOp::Add => OP_ADD.write_to(writer),
            AsmExprOp::Push(value) => {
                OP_PUSH.write_to(writer)?;
                value.write_to(writer)
            }
        }
    }
}

//===========================================================================//
