use super::error::{LinkError, LinkResult};
use super::types::{AbsoluteLabel, ChunkMetadata};
use crate::addr::Addr;
use crate::error::Errs;
use crate::expr::{ExprLabel, ExprValue};
use crate::obj::{ObjExpr, ObjExprOp};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// Represents the data needed to resolve symbols in a given evaluation context
/// during linking, which can either be an object file or the linker config.
pub(super) struct LinkSymbolContext<'a> {
    /// Metadata for each chunk in this evaluation context.  For an object
    /// file, this will be metadata for the chunks in that object file; for a
    /// linker config file, this will be empty.
    pub chunk_metadata: &'a [ChunkMetadata],
    /// For each symbol declared in this evaluation context, whether local or
    /// imported, this stores the address space and run address of the symbol,
    /// or `None` if the symbol couldn't be resolved (e.g. because it's an
    /// imported symbol that was never exported from anywhere), in which case
    /// an error has already been reported.
    pub symbol_addrs: HashMap<Rc<str>, Option<AbsoluteLabel>>,
}

impl<'a> LinkSymbolContext<'a> {
    fn resolve_label(&self, label: &ExprLabel) -> LinkResult<AbsoluteLabel> {
        match label {
            ExprLabel::AddrAbsolute { space, address } => Ok(AbsoluteLabel {
                space: space.clone(),
                address: Addr::wrap_bigint(address),
            }),
            &ExprLabel::ChunkAbsolute { chunk_index, ref address } => {
                if chunk_index >= self.chunk_metadata.len() {
                    // Reference to a chunk that doesn't exist in this object
                    // file.
                    return Err(Errs::one(
                        LinkError::MalformedPatchExpression,
                    ));
                }
                let metadata = &self.chunk_metadata[chunk_index];
                let space = metadata.start.space.clone();
                Ok(AbsoluteLabel {
                    space,
                    address: Addr::wrap_bigint(address),
                })
            }
            &ExprLabel::ChunkRelative { chunk_index, ref offset } => {
                if chunk_index >= self.chunk_metadata.len() {
                    // Reference to a chunk that doesn't exist in this object
                    // file.
                    return Err(Errs::one(
                        LinkError::MalformedPatchExpression,
                    ));
                }
                let metadata = &self.chunk_metadata[chunk_index];
                let chunk_start = metadata.start.clone();
                Ok(chunk_start.plus_offset(offset))
            }
            ExprLabel::SymbolRelative { name, offset } => {
                match self.symbol_addrs.get(name) {
                    None => {
                        // Reference to a symbol not declared in this object
                        // file.
                        Err(Errs::one(LinkError::MalformedPatchExpression))
                    }
                    Some(None) => {
                        // Imported symbol that was never exported; an error
                        // was already reported for this.
                        //
                        // TODO: Is there a better way to handle this than
                        // returning an empty error list?
                        Err(Errs::new())
                    }
                    Some(Some(symbol)) => Ok(symbol.plus_offset(offset)),
                }
            }
        }
    }
}

//===========================================================================//

pub(super) struct LinkEvalEnv {
    variables: Vec<ExprValue>,
}

impl LinkEvalEnv {
    pub fn new() -> Self {
        Self { variables: Vec::new() }
    }

    /// Pushes a new variable onto the variable stack.
    pub fn push_variable(&mut self, variable: ExprValue) {
        self.variables.push(variable);
    }

    /// Evaluates an expression and returns the result value.
    pub fn evaluate_expression(
        &self,
        expr: &ObjExpr,
        context: &LinkSymbolContext,
    ) -> LinkResult<ExprValue> {
        let mut expr_stack = Vec::<ExprValue>::new();
        for op in &expr.ops {
            match op {
                &ObjExprOp::BinOp(binop) => {
                    let opt_rhs = expr_stack.pop();
                    let opt_lhs = expr_stack.pop();
                    match (opt_lhs, opt_rhs) {
                        (Some(lhs), Some(rhs)) => {
                            match binop.evaluate(lhs, rhs) {
                                Ok(result) => expr_stack.push(result),
                                Err(_) => {
                                    // TODO: add error details
                                    return Err(Errs::one(
                                        LinkError::PatchEvaluationFailed,
                                    ));
                                }
                            }
                        }
                        _ => {
                            // Stack underflow.  That shouldn't happen if
                            // unless the object file was corrupted.
                            return Err(Errs::one(
                                LinkError::MalformedPatchExpression,
                            ));
                        }
                    }
                }
                &ObjExprOp::GetValue(index) => {
                    if index < self.variables.len() {
                        expr_stack.push(self.variables[index].clone());
                    } else {
                        // Invalid variable index.  That shouldn't happen if
                        // unless the object file was corrupted.
                        return Err(Errs::one(
                            LinkError::MalformedPatchExpression,
                        ));
                    }
                }
                ObjExprOp::LabelAddr => {
                    match expr_stack.pop() {
                        Some(ExprValue::Label(label)) => {
                            let resolved = context.resolve_label(&label)?;
                            let address = BigInt::from(resolved.address);
                            expr_stack.push(ExprValue::Integer(address));
                        }
                        _ => {
                            // The expression has a type error.  That shouldn't
                            // happen if unless the object file was corrupted.
                            return Err(Errs::one(
                                LinkError::MalformedPatchExpression,
                            ));
                        }
                    }
                }
                ObjExprOp::Push(ExprValue::Label(label)) => {
                    let resolved = context.resolve_label(label)?;
                    expr_stack.push(ExprValue::Label(
                        ExprLabel::AddrAbsolute {
                            space: resolved.space,
                            address: BigInt::from(resolved.address),
                        },
                    ));
                }
                ObjExprOp::Push(value) => expr_stack.push(value.clone()),
                other => todo!("{other:?}"),
            }
        }
        if let Some(value) = expr_stack.pop()
            && expr_stack.is_empty()
        {
            Ok(value)
        } else {
            Err(Errs::one(LinkError::MalformedPatchExpression))
        }
    }
}

//===========================================================================//
