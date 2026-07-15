use super::arch::ArchTree;
use super::error::{AsmError, AsmResult};
use crate::addr::Offset;
use crate::error::{Errs, SrcSpan};
use crate::expr::{
    ExprCompiler, ExprEnv, ExprFunc, ExprLabel, ExprType, ExprTypeError,
    ExprTypeResult, ExprValue,
};
use crate::obj::{ObjExpr, ObjExprOp, ObjPatch, ObjPatchData, ObjSymbol};
use crate::parse::{ExprAst, IdentifierAst, IdentifierKind};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) struct AsmTypeEnv {
    builtins: HashMap<Rc<str>, (ExprValue, ExprType)>,
    labels: HashMap<Rc<str>, SrcSpan>,
    arch_stack: Vec<Rc<str>>,
    chunk_stack: Vec<ChunkEnv>,
    scope_stack: Vec<ScopeEnv>,
}

impl AsmTypeEnv {
    pub fn new() -> AsmTypeEnv {
        let mut builtins = HashMap::<Rc<str>, (ExprValue, ExprType)>::new();
        let expr_type = ExprType::Function(Rc::new((
            ExprType::Integer,
            ExprType::Integer,
        )));
        for func in [ExprFunc::Cbrtz, ExprFunc::Sqrtz] {
            let value = ExprValue::Function(func);
            builtins.insert(Rc::from(func.name()), (value, expr_type.clone()));
        }
        AsmTypeEnv {
            builtins,
            labels: HashMap::new(),
            arch_stack: vec![Rc::from(ArchTree::ROOT_ARCH_NAME)],
            chunk_stack: Vec::new(),
            scope_stack: Vec::new(),
        }
    }

    pub fn declare_import(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        self.declare_label(id_ast)
    }

    pub fn declare_label(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        match id_ast.kind {
            IdentifierKind::Standard => {}
            IdentifierKind::Builtin => {
                return Err(Errs::one(AsmError::DeclNameIsBuiltin {
                    span: id_ast.span,
                    name: id_ast.name.clone(),
                }));
            }
            IdentifierKind::Placeholder => unreachable!(),
        }
        let full_name = if let Some(prefix) = self.current_scope_prefix() {
            Rc::from(format!("{prefix}{}", id_ast.name))
        } else {
            id_ast.name.clone()
        };
        if let Some(&prev_span) = self.labels.get(&full_name) {
            Err(Errs::one(AsmError::SymbolAlreadyDeclared {
                full_name,
                name_span: id_ast.span,
                prev_span,
            }))
        } else {
            self.labels.insert(full_name, id_ast.span);
            Ok(())
        }
    }

    pub fn begin_chunk(&mut self, chunk_index: usize) {
        self.chunk_stack.push(ChunkEnv::with_chunk_index(chunk_index));
        debug_assert!(!self.arch_stack.is_empty());
        self.arch_stack.push(self.arch_stack.last().unwrap().clone());
    }

    pub fn current_chunk(&mut self) -> Option<&mut ChunkEnv> {
        self.chunk_stack.last_mut()
    }

    pub fn end_chunk(&mut self) -> ChunkEnv {
        debug_assert!(self.arch_stack.len() >= 2);
        self.arch_stack.pop();
        debug_assert!(!self.chunk_stack.is_empty());
        self.chunk_stack.pop().unwrap()
    }

    pub fn current_arch(&self) -> &Rc<str> {
        debug_assert!(!self.arch_stack.is_empty());
        self.arch_stack.last().unwrap()
    }

    pub fn set_current_arch(&mut self, arch: Rc<str>) {
        debug_assert!(!self.arch_stack.is_empty());
        *self.arch_stack.last_mut().unwrap() = arch;
    }

    pub fn begin_scope(&mut self, name: &Rc<str>) {
        let prefix = if let Some(outer) = self.current_scope_prefix() {
            Rc::from(format!("{}{name}::", outer))
        } else {
            Rc::from(format!("{name}::"))
        };
        self.scope_stack.push(ScopeEnv { prefix });
    }

    fn current_scope_prefix(&self) -> Option<&str> {
        self.scope_stack.last().map(|scope| &*scope.prefix)
    }

    pub fn end_scope(&mut self) {
        debug_assert!(!self.scope_stack.is_empty());
        self.scope_stack.pop();
    }

    /// Returns the fully-qualified name for the symbol with the given name in
    /// the current scope, or `None` if no such symbol exists.
    pub fn look_up_symbol(&self, name: &str) -> Option<Rc<str>> {
        for scope in self.scope_stack.iter().rev() {
            let full = format!("{}{name}", scope.prefix);
            if let Some((full, _)) = self.labels.get_key_value(full.as_str()) {
                return Some(full.clone());
            }
        }
        if let Some((full, _)) = self.labels.get_key_value(name) {
            return Some(full.clone());
        }
        None
    }

    pub fn typecheck_expression(
        &self,
        expr: &ExprAst,
    ) -> AsmResult<(ObjExpr, ExprType)> {
        let (ops, expr_type, _static_value) =
            ExprCompiler::new(self).typecheck(expr).map_err(Errs::coerce)?;
        debug_assert!(!ops.is_empty());
        Ok((ObjExpr { ops }, expr_type))
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
            Err(Errs::one(ExprTypeError::RelativeLabelOutsideOfAnySection {
                span,
            }))
        }
    }

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, Option<ExprValue>)> {
        if let Some(full_name) = self.look_up_symbol(name) {
            let value = ExprValue::Label(ExprLabel::SymbolRelative {
                name: full_name,
                offset: BigInt::ZERO,
            });
            let op = ObjExprOp::Push(value.clone());
            return Ok((op, ExprType::Label, Some(value)));
        }
        if let Some((value, expr_type)) = self.builtins.get(name) {
            let op = ObjExprOp::Push(value.clone());
            return Ok((op, expr_type.clone(), Some(value.clone())));
        }
        Err(Errs::one(ExprTypeError::UnknownIdentifier {
            span,
            name: name.clone(),
        }))
    }
}

//===========================================================================//

pub(super) struct ChunkEnv {
    chunk_index: usize,
    data: Vec<u8>,
    padding: usize,
    patches: Vec<ObjPatch>,
    symbols: Vec<ObjSymbol>,
}

impl ChunkEnv {
    fn with_chunk_index(chunk_index: usize) -> ChunkEnv {
        ChunkEnv {
            chunk_index,
            data: Vec::new(),
            padding: 0,
            patches: Vec::new(),
            symbols: Vec::new(),
        }
    }

    pub fn total_size(&self) -> usize {
        // TODO: check for overflow
        self.data.len() + self.padding
    }

    pub fn data_mut(&mut self) -> &mut Vec<u8> {
        if self.padding > 0 {
            // TODO: If chunk has explicit fill byte, then no need for patch.
            self.add_patch(ObjPatch {
                // TODO: check for overflow
                offset: Offset::try_from(self.data.len()).unwrap(),
                data: ObjPatchData::Fill(self.padding),
            });
            self.data.resize(self.data.len() + self.padding, 0u8);
            self.padding = 0;
        }
        &mut self.data
    }

    pub fn add_padding(&mut self, padding: usize) {
        // TODO: check for overflow
        self.padding += padding;
    }

    pub fn add_patch(&mut self, patch: ObjPatch) {
        self.patches.push(patch);
    }

    pub fn add_symbol(&mut self, symbol: ObjSymbol) {
        self.symbols.push(symbol);
    }

    pub fn finish(self) -> FinishedChunk {
        FinishedChunk {
            data: Box::from(self.data),
            patches: Box::from(self.patches),
            symbols: Box::from(self.symbols),
        }
    }
}

//===========================================================================//

pub(super) struct FinishedChunk {
    pub data: Box<[u8]>,
    pub patches: Box<[ObjPatch]>,
    pub symbols: Box<[ObjSymbol]>,
}

//===========================================================================//

struct ScopeEnv {
    prefix: Rc<str>,
}

//===========================================================================//
