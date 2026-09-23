use super::config::ConfigVariableOr;
use super::error::{LinkError, LinkResult};
use super::types::{AbsoluteLabel, ChunkMetadata};
use crate::addr::Addr;
use crate::error::Errs;
use crate::expr::{ExprEvalError, ExprFunc, ExprLabel, ExprValue};
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
                        LinkError::MalformedPatchExpression {
                            message: Rc::from("ChunkAbsolute: no such chunk"),
                        },
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
                        LinkError::MalformedPatchExpression {
                            message: Rc::from("ChunkRelative: no such chunk"),
                        },
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
                        let message =
                            format!("SymbolRelative: no such symbol: {name}");
                        Err(Errs::one(LinkError::MalformedPatchExpression {
                            message: Rc::from(message),
                        }))
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
            Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("invalid variable index"),
            }))
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
        ExprEvaluator::new(self, expr, context).evaluate()
    }
}

//===========================================================================//

struct ExprEvaluator<'a> {
    env: &'a LinkEvalEnv,
    symbol_context: &'a LinkSymbolContext<'a>,
    ops: &'a [ObjExprOp],
    op_index: usize,
    value_stack: Vec<ExprValue>,
}

impl<'a> ExprEvaluator<'a> {
    pub fn new(
        env: &'a LinkEvalEnv,
        expr: &'a ObjExpr,
        symbol_context: &'a LinkSymbolContext,
    ) -> Self {
        Self {
            env,
            symbol_context,
            ops: &expr.ops,
            op_index: 0,
            value_stack: Vec::new(),
        }
    }

    pub fn evaluate(mut self) -> LinkResult<ExprValue> {
        while self.op_index < self.ops.len() {
            let op = &self.ops[self.op_index];
            self.op_index += 1;
            match op {
                ObjExprOp::Apply { context, arg_span } => {
                    let arg_value = self.pop_value()?;
                    let func = self.pop_func()?;
                    let ret_value = func.call(arg_value).map_err(|error| {
                        Errs::one(LinkError::ExprEvalError {
                            context: context.clone(),
                            error: ExprEvalError::FuncEvalError {
                                arg_span: *arg_span,
                                error,
                            },
                        })
                    })?;
                    self.push_value(ret_value);
                }
                ObjExprOp::BinOp {
                    context,
                    binop,
                    op_span,
                    lhs_span,
                    rhs_span,
                } => {
                    let rhs = self.pop_value()?;
                    let lhs = self.pop_value()?;
                    let ret_value =
                        binop.evaluate(lhs, rhs).map_err(|error| {
                            Errs::one(LinkError::ExprEvalError {
                                context: context.clone(),
                                error: error.into_expr_eval_error(
                                    *op_span, *lhs_span, *rhs_span,
                                ),
                            })
                        })?;
                    self.push_value(ret_value);
                }
                &ObjExprOp::GetValue(index) => {
                    let value = self.env.get_variable(index)?;
                    self.push_value(value.clone());
                }
                ObjExprOp::Interpolate(template) => {
                    let arg = self.pop_value()?;
                    let string = template.format(arg).map_err(|_| {
                        // Type error.  That shouldn't happen unless the object
                        // file was corrupted.
                        Errs::one(LinkError::MalformedPatchExpression {
                            message: Rc::from("Interpolate type error"),
                        })
                    })?;
                    self.push_value(ExprValue::String(string));
                }
                ObjExprOp::ListIndex { context, list_span, index_span } => {
                    let index = self.pop_int()?;
                    let items = self.pop_list()?;
                    if index < BigInt::ZERO
                        || index >= BigInt::from(items.len())
                    {
                        return Err(Errs::one(LinkError::ExprEvalError {
                            context: context.clone(),
                            error: ExprEvalError::ListIndexOutOfBounds {
                                list_span: *list_span,
                                list_length: items.len(),
                                index_span: *index_span,
                                index_value: index,
                            },
                        }));
                    }
                    let item = items[usize::try_from(index).unwrap()].clone();
                    self.push_value(item);
                }
                &ObjExprOp::MakeList(num_items) => {
                    let items = self.pop_values(num_items)?;
                    self.push_value(ExprValue::List(Rc::from(items)));
                }
                &ObjExprOp::MakeTuple(num_items) => {
                    let items = self.pop_values(num_items)?;
                    self.push_value(ExprValue::Tuple(Rc::from(items)));
                }
                ObjExprOp::Push(ExprValue::Label(label)) => {
                    let resolved = self.symbol_context.resolve_label(label)?;
                    self.push_value(ExprValue::Label(
                        ExprLabel::AddrAbsolute {
                            space: resolved.space,
                            address: BigInt::from(resolved.address),
                        },
                    ));
                }
                ObjExprOp::Push(value) => self.push_value(value.clone()),
                &ObjExprOp::Skip(offset) => self.skip(offset)?,
                &ObjExprOp::SkipIf(offset) => {
                    if self.pop_bool()? {
                        self.skip(offset)?;
                    }
                }
                &ObjExprOp::SkipUnless(offset) => {
                    if !self.pop_bool()? {
                        self.skip(offset)?;
                    }
                }
                &ObjExprOp::TupleItem(index) => {
                    let items = self.pop_tuple()?;
                    if index >= items.len() {
                        // Type error.  That shouldn't happen unless the object
                        // file was corrupted.
                        return Err(Errs::one(
                            LinkError::MalformedPatchExpression {
                                message: Rc::from("TupleItem index error"),
                            },
                        ));
                    }
                    self.push_value(items[index].clone());
                }
                ObjExprOp::UnOp { context, unop, op_span, arg_span } => {
                    let arg = self.pop_value()?;
                    match unop.evaluate(arg) {
                        Ok(result) => self.push_value(result),
                        Err(error) => {
                            return Err(Errs::one(LinkError::ExprEvalError {
                                context: context.clone(),
                                error: error
                                    .into_expr_eval_error(*op_span, *arg_span),
                            }));
                        }
                    }
                }
            }
        }
        let value = self.pop_value()?;
        if !self.value_stack.is_empty() {
            // More than one value left on the stack at the end.  That
            // shouldn't happen unless the object file was corrupted.
            return Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("more than one expr stack item"),
            }));
        }
        Ok(value)
    }

    fn push_value(&mut self, value: ExprValue) {
        self.value_stack.push(value);
    }

    fn pop_values(&mut self, num_items: usize) -> LinkResult<Vec<ExprValue>> {
        if self.value_stack.len() >= num_items {
            let start = self.value_stack.len() - num_items;
            Ok(self.value_stack.split_off(start))
        } else {
            // Stack underflow.  That shouldn't happen unless the object
            // file was corrupted.
            Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("pop_values stack underflow"),
            }))
        }
    }

    fn pop_value(&mut self) -> LinkResult<ExprValue> {
        match self.value_stack.pop() {
            Some(value) => Ok(value),
            // Stack underflow.  That shouldn't happen unless the object file
            // was corrupted.
            None => Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("pop_value stack underflow"),
            })),
        }
    }

    fn pop_bool(&mut self) -> LinkResult<bool> {
        match self.pop_value()? {
            ExprValue::Boolean(boolean) => Ok(boolean),
            // Type error.  That shouldn't happen unless the object file was
            // corrupted.
            _ => Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("pop_bool type error"),
            })),
        }
    }

    fn pop_func(&mut self) -> LinkResult<ExprFunc> {
        match self.pop_value()? {
            ExprValue::Function(func) => Ok(func),
            // Type error.  That shouldn't happen unless the object file was
            // corrupted.
            _ => Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("pop_func type error"),
            })),
        }
    }

    fn pop_int(&mut self) -> LinkResult<BigInt> {
        match self.pop_value()? {
            ExprValue::Integer(bigint) => Ok(bigint),
            // Type error.  That shouldn't happen unless the object file was
            // corrupted.
            _ => Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("pop_int type error"),
            })),
        }
    }

    fn pop_list(&mut self) -> LinkResult<Rc<[ExprValue]>> {
        match self.pop_value()? {
            ExprValue::List(items) => Ok(items),
            // Type error.  That shouldn't happen unless the object file was
            // corrupted.
            _ => Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("pop_list type error"),
            })),
        }
    }

    fn pop_tuple(&mut self) -> LinkResult<Rc<[ExprValue]>> {
        match self.pop_value()? {
            ExprValue::Tuple(items) => Ok(items),
            // Type error.  That shouldn't happen unless the object file was
            // corrupted.
            _ => Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("pop_tuple type error"),
            })),
        }
    }

    fn skip(&mut self, offset: usize) -> LinkResult<()> {
        self.op_index = self.op_index.saturating_add(offset);
        if self.op_index <= self.ops.len() {
            Ok(())
        } else {
            // Op index overflow.  That shouldn't happen unless the object file
            // was corrupted.
            Err(Errs::one(LinkError::MalformedPatchExpression {
                message: Rc::from("skip index overflow"),
            }))
        }
    }
}

//===========================================================================//
