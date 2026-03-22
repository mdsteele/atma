use crate::error::{SourceError, SourceResult, SrcSpan};
use crate::expr::{
    ExprCompiler, ExprEnv, ExprLabel, ExprType, ExprTypeError, ExprTypeResult,
    ExprValue,
};
use crate::obj::{ObjExpr, ObjExprOp, ObjPatch, ObjSymbol};
use crate::parse::{ExprAst, IdentifierAst};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) struct AsmTypeEnv {
    labels: HashMap<Rc<str>, SrcSpan>,
    chunk_stack: Vec<ChunkEnv>,
}

impl AsmTypeEnv {
    pub fn new() -> AsmTypeEnv {
        AsmTypeEnv { labels: HashMap::new(), chunk_stack: Vec::new() }
    }

    pub fn declare_import(
        &mut self,
        id_ast: &IdentifierAst,
    ) -> SourceResult<()> {
        self.declare_label(id_ast)
    }

    pub fn declare_label(
        &mut self,
        id_ast: &IdentifierAst,
    ) -> SourceResult<()> {
        debug_assert!(!id_ast.is_placeholder);
        if let Some(&prev_span) = self.labels.get(&id_ast.name) {
            let message =
                format!("symbol was already declared: {}", id_ast.name);
            let label1 = "previously declared here".to_string();
            let label2 = "redeclared here".to_string();
            Err(vec![
                SourceError::new(id_ast.span, message)
                    .with_label(prev_span, label1)
                    .with_label(id_ast.span, label2),
            ])
        } else {
            self.labels.insert(id_ast.name.clone(), id_ast.span);
            Ok(())
        }
    }

    pub fn begin_chunk(&mut self, chunk_index: usize) {
        self.chunk_stack.push(ChunkEnv::with_chunk_index(chunk_index));
    }

    pub fn current_chunk(&mut self) -> Option<&mut ChunkEnv> {
        self.chunk_stack.last_mut()
    }

    pub fn end_chunk(&mut self) -> ChunkEnv {
        debug_assert!(!self.chunk_stack.is_empty());
        self.chunk_stack.pop().unwrap()
    }

    pub fn typecheck_expression(
        &self,
        expr: &ExprAst,
    ) -> SourceResult<(ObjExpr, ExprType)> {
        match ExprCompiler::new(self).typecheck(expr) {
            Ok((ops, expr_type, _static_value)) => {
                debug_assert!(!ops.is_empty());
                Ok((ObjExpr { ops }, expr_type))
            }
            Err(errors) => Err(SourceError::from_errors(errors)),
        }
    }
}

impl ExprEnv for AsmTypeEnv {
    type Op = ObjExprOp;

    fn typecheck_here_label(
        &self,
        span: SrcSpan,
    ) -> ExprTypeResult<(Self::Op, Option<ExprValue>)> {
        if let Some(chunk_env) = self.chunk_stack.last() {
            let chunk_index = chunk_env.chunk_index;
            let offset = BigInt::from(chunk_env.data.len());
            let value = ExprValue::Label(ExprLabel::ChunkRelative {
                chunk_index,
                offset,
            });
            let op = ObjExprOp::Push(value.clone());
            Ok((op, Some(value)))
        } else {
            Err(vec![ExprTypeError::RelativeLabelOutsideOfAnySection { span }])
        }
    }

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, Option<ExprValue>)> {
        if self.labels.contains_key(name) {
            let value = ExprValue::Label(ExprLabel::SymbolRelative {
                name: name.clone(),
                offset: BigInt::ZERO,
            });
            let op = ObjExprOp::Push(value.clone());
            Ok((op, ExprType::Label, Some(value)))
        } else {
            Err(vec![ExprTypeError::UnknownIdentifier {
                span,
                name: name.clone(),
            }])
        }
    }
}

//===========================================================================//

pub(super) struct ChunkEnv {
    pub chunk_index: usize,
    pub data: Vec<u8>,
    pub symbols: Vec<ObjSymbol>,
    pub patches: Vec<ObjPatch>,
}

impl ChunkEnv {
    fn with_chunk_index(chunk_index: usize) -> ChunkEnv {
        ChunkEnv {
            chunk_index,
            data: Vec::new(),
            symbols: Vec::new(),
            patches: Vec::new(),
        }
    }
}

//===========================================================================//
