use super::config::ConfigVariableOr;
use super::error::{LinkError, LinkResult};
use super::types::{AbsoluteLabel, ChunkMetadata};
use crate::addr::Addr;
use crate::error::Errs;
use crate::expr::{ExprEvalError, ExprLabel, ExprValue};
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
    pub fn resolve_label(
        &self,
        label: &ExprLabel,
    ) -> LinkResult<AbsoluteLabel> {
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
    /// Evaluated variable values, or `None` for variables whose evaluation
    /// failed.
    variables: Vec<Option<ExprValue>>,
}

impl LinkEvalEnv {
    pub fn new() -> Self {
        Self { variables: Vec::new() }
    }

    /// Evaluates an expression and pushes the result onto the variable stack.
    pub fn evaluate_variable(
        &mut self,
        expr: &ObjExpr,
        context: &LinkSymbolContext,
    ) -> LinkResult<()> {
        match self.evaluate_expression(expr, context) {
            Ok(value) => {
                self.variables.push(Some(value));
                Ok(())
            }
            Err(errs) => {
                self.variables.push(None);
                Err(errs)
            }
        }
    }

    fn get_variable(&self, index: usize) -> LinkResult<&ExprValue> {
        if index < self.variables.len() {
            // If `self.variables[index]` is None, then evaluating that
            // variable failed, and an error was already reported, so we should
            // fail silently here.
            self.variables[index].as_ref().ok_or_else(Errs::new)
        } else {
            // Invalid variable index.  That shouldn't happen if
            // unless the object file was corrupted.
            Err(Errs::one(LinkError::MalformedPatchExpression))
        }
    }

    pub fn resolve<T, F>(
        &self,
        variable: ConfigVariableOr<T>,
        func: F,
    ) -> LinkResult<T>
    where
        F: FnOnce(&ExprValue) -> LinkResult<T>,
    {
        match variable {
            ConfigVariableOr::Variable(index) => {
                func(self.get_variable(index)?)
            }
            ConfigVariableOr::Static(value) => Ok(value),
        }
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
                ObjExprOp::Apply { context, arg_span } => {
                    let arg_value = pop_value(&mut expr_stack)?;
                    let func_value = pop_value(&mut expr_stack)?;
                    if let ExprValue::Function(func) = func_value {
                        match func.call(arg_value) {
                            Ok(result_value) => expr_stack.push(result_value),
                            Err(error) => {
                                return Err(Errs::one(
                                    LinkError::ExprEvalError {
                                        context: context.clone(),
                                        error: ExprEvalError::FuncEvalError {
                                            arg_span: *arg_span,
                                            error,
                                        },
                                    },
                                ));
                            }
                        }
                    } else {
                        // Type error.  That shouldn't happen unless the
                        // object file was corrupted.
                        return Err(Errs::one(
                            LinkError::MalformedPatchExpression,
                        ));
                    }
                }
                ObjExprOp::BinOp {
                    context,
                    binop,
                    op_span,
                    lhs_span,
                    rhs_span,
                } => {
                    let rhs = pop_value(&mut expr_stack)?;
                    let lhs = pop_value(&mut expr_stack)?;
                    match binop.evaluate(lhs, rhs) {
                        Ok(result) => expr_stack.push(result),
                        Err(error) => {
                            return Err(Errs::one(LinkError::ExprEvalError {
                                context: context.clone(),
                                error: error.into_expr_eval_error(
                                    *op_span, *lhs_span, *rhs_span,
                                ),
                            }));
                        }
                    }
                }
                &ObjExprOp::GetValue(index) => {
                    let value = self.get_variable(index)?;
                    expr_stack.push(value.clone());
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
                ObjExprOp::UnOp { context, unop, op_span, arg_span } => {
                    let arg = pop_value(&mut expr_stack)?;
                    match unop.evaluate(arg) {
                        Ok(result) => expr_stack.push(result),
                        Err(error) => {
                            return Err(Errs::one(LinkError::ExprEvalError {
                                context: context.clone(),
                                error: error
                                    .into_expr_eval_error(*op_span, *arg_span),
                            }));
                        }
                    }
                }
                other => todo!("{other:?}"),
            }
        }
        let value = pop_value(&mut expr_stack)?;
        if !expr_stack.is_empty() {
            // More than one value left on the stack at the end.  That
            // shouldn't happen unless the object file was corrupted.
            return Err(Errs::one(LinkError::MalformedPatchExpression));
        }
        Ok(value)
    }
}

fn pop_value(expr_stack: &mut Vec<ExprValue>) -> LinkResult<ExprValue> {
    match expr_stack.pop() {
        Some(value) => Ok(value),
        None => {
            // Stack underflow.  That shouldn't happen unless
            // the object file was corrupted.
            Err(Errs::one(LinkError::MalformedPatchExpression))
        }
    }
}

//===========================================================================//
