use super::error::{AsmError, AsmResult};
use crate::error::{Errs, SrcSpan};
use crate::parse::{
    AsmAssertAst, AsmDefMacroAst, AsmIntDataAst, AsmInvokeAst, AsmMacroArgAst,
    AsmStmtAst, ExprAst, ExprAstNode, IdentifierAst, IdentifierKind, Token,
    TokenValue,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

//===========================================================================//

pub(super) struct MacroTable {
    definitions: HashMap<MacroSignature, Vec<MacroDefinition>>,
}

impl MacroTable {
    pub fn new() -> MacroTable {
        MacroTable { definitions: HashMap::new() }
    }

    pub fn expand(
        &self,
        arches: &[Rc<str>],
        invoke_ast: AsmInvokeAst,
    ) -> AsmResult<Vec<AsmStmtAst>> {
        let name = normalize_macro_name(&invoke_ast.id.name);
        for arch in arches {
            let signature = MacroSignature {
                arch: arch.clone(),
                name: name.clone(),
                num_args: invoke_ast.args.len(),
            };
            if let Some(definitions) = self.definitions.get(&signature) {
                for definition in definitions.iter().rev() {
                    match definition.try_expand(&invoke_ast.args) {
                        Err(MacroError::FailedToMatchPattern) => continue,
                        Err(MacroError::FailedToParseArguments(errors)) => {
                            return Err(errors);
                        }
                        Ok(stmts) => return Ok(stmts),
                    }
                }
            }
        }
        // TODO: Better error messages; for example, report if a macro name
        // exists, but needs a differenct pattern, or if the macro name exists
        // under a different architecture.
        let arch = arches.first().cloned().unwrap_or_default();
        Err(Errs::one(AsmError::UnmatchedMacroInvocation {
            macro_name: name,
            arch,
            invocation_span: invoke_ast.id.span,
        }))
    }

    pub fn define(
        &mut self,
        arch: &Rc<str>,
        reserved: &HashSet<Rc<str>>,
        def_macro_ast: AsmDefMacroAst,
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let num_args = def_macro_ast.params.len();
        let mut builder = errs
            .with(MacroBuilder::with_params(def_macro_ast.params, reserved));
        errs.also(builder.scan_statements(&def_macro_ast.body));
        let Some(params) = errs.ok(builder.build()) else {
            return Err(errs);
        };
        errs.result()?;
        let definition = MacroDefinition { params, body: def_macro_ast.body };
        let signature = MacroSignature {
            arch: arch.clone(),
            name: normalize_macro_name(&def_macro_ast.id.name),
            num_args,
        };
        self.definitions.entry(signature).or_default().push(definition);
        Ok(())
    }
}

//===========================================================================//

/// Helper type for `MacroTable::define`.
struct MacroBuilder<'a> {
    params: Vec<AsmMacroArgAst>,
    placeholders: HashMap<Rc<str>, PlaceholderKind>,
    reserved: &'a HashSet<Rc<str>>,
}

impl<'a> MacroBuilder<'a> {
    fn with_params(
        params: Vec<AsmMacroArgAst>,
        reserved: &'a HashSet<Rc<str>>,
    ) -> (MacroBuilder<'a>, Errs<AsmError>) {
        let mut placeholders = HashMap::<Rc<str>, SrcSpan>::new();
        let mut errs = Errs::<AsmError>::new();
        for param in &params {
            for token in &param.tokens {
                if let TokenValue::Placeholder(name) = &token.value {
                    if let Some(&prev_span) = placeholders.get(name) {
                        errs.push(AsmError::DuplicateMacroPlaceholder {
                            placeholder_name: name.clone(),
                            placeholder_span: token.span,
                            prev_span,
                        });
                    } else {
                        placeholders.insert(name.clone(), token.span);
                    }
                }
            }
        }
        let placeholders = placeholders
            .into_keys()
            .map(|name| (name, PlaceholderKind::default()))
            .collect();
        (MacroBuilder { params, placeholders, reserved }, errs)
    }

    fn scan_statements(&mut self, statements: &[AsmStmtAst]) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        for stmt in statements {
            errs.also(self.scan_statement(stmt));
        }
        errs.result()
    }

    fn scan_statement(&mut self, statement: &AsmStmtAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        match statement {
            AsmStmtAst::Assert(assert) => {
                errs.also(self.scan_expression(&assert.condition));
                if let Some(message) = &assert.message {
                    errs.also(self.scan_expression(message));
                }
            }
            AsmStmtAst::Import(id) | AsmStmtAst::Label(id) => {
                errs.also(self.scan_identifier(id));
            }
            AsmStmtAst::IntData(int_data) => {
                errs.also(self.scan_expressions(&int_data.expressions));
            }
            other => todo!("scan_statement {other:?}"), // TODO
        }
        errs.result()
    }

    fn scan_expressions(&mut self, expressions: &[ExprAst]) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        for expr in expressions {
            errs.also(self.scan_expression(expr));
        }
        errs.result()
    }

    fn scan_expression(&mut self, expression: &ExprAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        match &expression.node {
            ExprAstNode::Placeholder(name) => {
                errs.also(self.unify_placeholder(
                    expression.span,
                    name,
                    PlaceholderKind::Expression,
                ));
            }
            ExprAstNode::BoolLiteral(_)
            | ExprAstNode::HereLabel
            | ExprAstNode::Identifier(_)
            | ExprAstNode::IntLiteral(_)
            | ExprAstNode::StrLiteral(_) => {}
            ExprAstNode::UnOp(_, subexpr) => {
                errs.also(self.scan_expression(subexpr));
            }
            ExprAstNode::Apply(lhs, rhs)
            | ExprAstNode::BinOp(_, lhs, rhs)
            | ExprAstNode::Index(_, lhs, rhs) => {
                errs.also(self.scan_expression(lhs));
                errs.also(self.scan_expression(rhs));
            }
            ExprAstNode::Conditional(pred, lhs, rhs) => {
                errs.also(self.scan_expression(pred));
                errs.also(self.scan_expression(lhs));
                errs.also(self.scan_expression(rhs));
            }
            ExprAstNode::ListLiteral(items)
            | ExprAstNode::TupleLiteral(items) => {
                for item in items {
                    errs.also(self.scan_expression(item));
                }
            }
        }
        errs.result()
    }

    fn scan_identifier(
        &mut self,
        identifier: &IdentifierAst,
    ) -> AsmResult<()> {
        match identifier.kind {
            IdentifierKind::Placeholder => self.unify_placeholder(
                identifier.span,
                &identifier.name,
                PlaceholderKind::Identifier,
            ),
            _ => Ok(()),
        }
    }

    fn unify_placeholder(
        &mut self,
        span: SrcSpan,
        name: &Rc<str>,
        requirement: PlaceholderKind,
    ) -> AsmResult<()> {
        if let Some(kind) = self.placeholders.get_mut(name) {
            kind.unify_with(requirement);
            Ok(())
        } else {
            Err(Errs::one(AsmError::UnknownMacroPlaceholder {
                name: name.clone(),
                span,
            }))
        }
    }

    fn build(self) -> AsmResult<Vec<MacroParameter>> {
        let mut errs = Errs::<AsmError>::new();
        let mut params =
            Vec::<MacroParameter>::with_capacity(self.params.len());
        for param in self.params {
            let placeholders = param
                .tokens
                .iter()
                .enumerate()
                .filter_map(|(i, token)| match &token.value {
                    TokenValue::Placeholder(name) => Some((i, name)),
                    _ => None,
                })
                .collect::<Vec<(usize, &Rc<str>)>>();
            params.push(match placeholders[..] {
                [] => MacroParameter::Exact {
                    pattern: param
                        .tokens
                        .into_iter()
                        .map(|token| param_token(&token, self.reserved))
                        .collect(),
                },
                [(index, name)] => {
                    let suffix = param.tokens[(index + 1)..]
                        .iter()
                        .map(|token| param_token(token, self.reserved))
                        .collect();
                    let prefix = param.tokens[..index]
                        .iter()
                        .map(|token| param_token(token, self.reserved))
                        .collect();
                    MacroParameter::Placeholder {
                        prefix,
                        kind: self.placeholders[name],
                        name: name.clone(),
                        suffix,
                    }
                }
                _ => {
                    errs.push(AsmError::MultipleMacroPlaceholders {
                        span: param.span,
                    });
                    continue;
                }
            });
        }
        errs.result()?;
        Ok(params)
    }
}

fn param_token(token: &Token, reserved: &HashSet<Rc<str>>) -> MacroParamToken {
    if let TokenValue::Identifier(name) = &token.value {
        let res = Rc::<str>::from(name.to_ascii_uppercase());
        if reserved.contains(&res) {
            return MacroParamToken::Reserved(res);
        }
    }
    MacroParamToken::Exact(token.value.clone())
}

//===========================================================================//

/// Helper type for `MacroBuilder`.  Describes what can be substituted for a
/// given placeholder.  More restrictive types compare greater.
#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd)]
enum PlaceholderKind {
    #[default]
    Expression,
    // TODO: LValue
    Identifier,
}

impl PlaceholderKind {
    fn unify_with(&mut self, requirement: PlaceholderKind) {
        *self = requirement.max(*self);
    }
}

//===========================================================================//

#[derive(Clone, Hash, Eq, PartialEq)]
struct MacroSignature {
    pub arch: Rc<str>,
    pub name: Rc<str>,
    pub num_args: usize,
}

//===========================================================================//

enum MacroError {
    FailedToMatchPattern,
    FailedToParseArguments(Errs<AsmError>),
}

type MacroResult<T> = Result<T, MacroError>;

//===========================================================================//

struct MacroDefinition {
    params: Vec<MacroParameter>,
    body: Vec<AsmStmtAst>,
}

impl MacroDefinition {
    pub fn try_expand(
        &self,
        args: &[AsmMacroArgAst],
    ) -> MacroResult<Vec<AsmStmtAst>> {
        let expansion = MacroExpansion::try_match(&self.params, args)?;
        Ok(expansion.expand_statements(&self.body))
    }
}

//===========================================================================//

enum MacroParameter {
    Exact {
        pattern: Vec<MacroParamToken>,
    },
    Placeholder {
        prefix: Vec<MacroParamToken>,
        kind: PlaceholderKind,
        name: Rc<str>,
        suffix: Vec<MacroParamToken>,
    },
}

impl MacroParameter {
    pub fn try_match<'a>(
        &self,
        arg: &'a AsmMacroArgAst,
    ) -> MacroResult<MacroArgument<'a>> {
        match self {
            MacroParameter::Exact { pattern } => {
                try_match_tokens(&arg.tokens, pattern)?;
                Ok(MacroArgument::Exact)
            }
            MacroParameter::Placeholder { prefix, kind, name, suffix } => {
                if arg.tokens.len() <= prefix.len() + suffix.len() {
                    return Err(MacroError::FailedToMatchPattern);
                }
                try_match_tokens(&arg.tokens[..prefix.len()], prefix)?;
                let suffix_start = arg.tokens.len() - suffix.len();
                try_match_tokens(&arg.tokens[suffix_start..], suffix)?;
                let inner_tokens = &arg.tokens[prefix.len()..suffix_start];
                Ok(MacroArgument::Placeholder {
                    kind: *kind,
                    name: name.clone(),
                    tokens: inner_tokens,
                })
            }
        }
    }
}

fn try_match_tokens(
    tokens: &[Token],
    pattern: &[MacroParamToken],
) -> MacroResult<()> {
    if tokens.len() != pattern.len() {
        return Err(MacroError::FailedToMatchPattern);
    }
    for (token, pat) in tokens.iter().zip(pattern.iter()) {
        if !pat.matches(token) {
            return Err(MacroError::FailedToMatchPattern);
        }
    }
    Ok(())
}

//===========================================================================//

/// A specification, as part of macro definition parameter, for a matching a
/// single token in an argument in a macro invocation.
enum MacroParamToken {
    /// The token must have exactly the specified value.
    Exact(TokenValue),
    /// The token must be an identifier that matches the specified reserved
    /// name case-insensitively.
    Reserved(Rc<str>),
}

impl MacroParamToken {
    pub fn matches(&self, token: &Token) -> bool {
        match self {
            MacroParamToken::Exact(value) => &token.value == value,
            MacroParamToken::Reserved(res) => match &token.value {
                TokenValue::Identifier(name) => {
                    **res == *name.to_ascii_uppercase()
                }
                _ => false,
            },
        }
    }
}

//===========================================================================//

/// One argument to a macro invocation, after it has been matched against the
/// corresponding parameter of the macro definition.
enum MacroArgument<'a> {
    Exact,
    Placeholder { kind: PlaceholderKind, name: Rc<str>, tokens: &'a [Token] },
}

//===========================================================================//

enum MacroSubstitution {
    Expression(ExprAst),
    Identifier(IdentifierAst),
}

impl MacroSubstitution {
    fn parse_tokens(
        kind: PlaceholderKind,
        tokens: &[Token],
    ) -> AsmResult<MacroSubstitution> {
        match kind {
            PlaceholderKind::Expression => {
                let expr = ExprAst::parse(tokens).map_err(Errs::coerce)?;
                Ok(MacroSubstitution::Expression(expr))
            }
            PlaceholderKind::Identifier => {
                let id = IdentifierAst::parse(tokens).map_err(Errs::coerce)?;
                Ok(MacroSubstitution::Identifier(id))
            }
        }
    }

    fn unwrap_expression(&self) -> ExprAst {
        match self {
            MacroSubstitution::Expression(expr) => expr.clone(),
            MacroSubstitution::Identifier(id) => ExprAst {
                span: id.span,
                node: ExprAstNode::Identifier(id.name.clone()),
            },
        }
    }

    fn unwrap_identifier(&self) -> IdentifierAst {
        match self {
            MacroSubstitution::Identifier(id) => id.clone(),
            _ => panic!("unwrap_identifier"),
        }
    }
}

//===========================================================================//

struct MacroExpansion {
    subs: HashMap<Rc<str>, MacroSubstitution>,
}

impl MacroExpansion {
    fn try_match(
        params: &[MacroParameter],
        args: &[AsmMacroArgAst],
    ) -> MacroResult<MacroExpansion> {
        if args.len() != params.len() {
            return Err(MacroError::FailedToMatchPattern);
        }
        let matched = args
            .iter()
            .zip(params.iter())
            .map(|(arg, param)| param.try_match(arg))
            .collect::<MacroResult<Vec<MacroArgument>>>()?;
        let mut subs = HashMap::<Rc<str>, MacroSubstitution>::new();
        let mut errs = Errs::<AsmError>::new();
        for macro_arg in matched {
            match macro_arg {
                MacroArgument::Exact => {}
                MacroArgument::Placeholder { kind, name, tokens } => {
                    if let Some(sub) =
                        errs.ok(MacroSubstitution::parse_tokens(kind, tokens))
                    {
                        subs.insert(name, sub);
                    }
                }
            }
        }
        if errs.is_empty() {
            Ok(MacroExpansion { subs })
        } else {
            Err(MacroError::FailedToParseArguments(errs))
        }
    }

    fn expand_statements(&self, statements: &[AsmStmtAst]) -> Vec<AsmStmtAst> {
        statements.iter().map(|stmt| self.expand_statement(stmt)).collect()
    }

    fn expand_statement(&self, statement: &AsmStmtAst) -> AsmStmtAst {
        match statement {
            AsmStmtAst::Assert(assert) => AsmStmtAst::Assert(AsmAssertAst {
                condition: self.expand_expression(&assert.condition),
                message: assert
                    .message
                    .as_ref()
                    .map(|expr| self.expand_expression(expr)),
            }),
            AsmStmtAst::Import(id) => {
                AsmStmtAst::Import(self.expand_identifier(id))
            }
            AsmStmtAst::IntData(int_data) => {
                AsmStmtAst::IntData(AsmIntDataAst {
                    directive_span: int_data.directive_span,
                    int_type: int_data.int_type,
                    expressions: self
                        .expand_expressions(&int_data.expressions),
                })
            }
            AsmStmtAst::Label(id) => {
                AsmStmtAst::Label(self.expand_identifier(id))
            }
            other => todo!("macro expand {other:?}"), // TODO
        }
    }

    fn expand_expressions(&self, expressions: &[ExprAst]) -> Vec<ExprAst> {
        expressions.iter().map(|expr| self.expand_expression(expr)).collect()
    }

    fn expand_expression(&self, expression: &ExprAst) -> ExprAst {
        match &expression.node {
            ExprAstNode::Apply(lhs, rhs) => ExprAst {
                span: expression.span,
                node: ExprAstNode::Apply(
                    Box::from(self.expand_expression(lhs)),
                    Box::from(self.expand_expression(rhs)),
                ),
            },
            ExprAstNode::BinOp(op, lhs, rhs) => ExprAst {
                span: expression.span,
                node: ExprAstNode::BinOp(
                    *op,
                    Box::from(self.expand_expression(lhs)),
                    Box::from(self.expand_expression(rhs)),
                ),
            },
            ExprAstNode::BoolLiteral(_)
            | ExprAstNode::HereLabel
            | ExprAstNode::Identifier(_)
            | ExprAstNode::IntLiteral(_)
            | ExprAstNode::StrLiteral(_) => expression.clone(),
            ExprAstNode::Conditional(pred, lhs, rhs) => ExprAst {
                span: expression.span,
                node: ExprAstNode::Conditional(
                    Box::from(self.expand_expression(pred)),
                    Box::from(self.expand_expression(lhs)),
                    Box::from(self.expand_expression(rhs)),
                ),
            },
            ExprAstNode::Index(index_span, lhs, rhs) => ExprAst {
                span: expression.span,
                node: ExprAstNode::Index(
                    *index_span,
                    Box::from(self.expand_expression(lhs)),
                    Box::from(self.expand_expression(rhs)),
                ),
            },
            ExprAstNode::ListLiteral(exprs) => ExprAst {
                span: expression.span,
                node: ExprAstNode::ListLiteral(self.expand_expressions(exprs)),
            },
            ExprAstNode::Placeholder(name) => {
                if let Some(substitution) = self.subs.get(name) {
                    substitution.unwrap_expression()
                } else {
                    expression.clone()
                }
            }
            ExprAstNode::TupleLiteral(exprs) => ExprAst {
                span: expression.span,
                node: ExprAstNode::TupleLiteral(
                    self.expand_expressions(exprs),
                ),
            },
            ExprAstNode::UnOp(op, subexpr) => ExprAst {
                span: expression.span,
                node: ExprAstNode::UnOp(
                    *op,
                    Box::from(self.expand_expression(subexpr)),
                ),
            },
        }
    }

    fn expand_identifier(&self, identifier: &IdentifierAst) -> IdentifierAst {
        if let Some(substitution) = self.subs.get(&identifier.name) {
            substitution.unwrap_identifier()
        } else {
            identifier.clone()
        }
    }
}

//===========================================================================//

fn normalize_macro_name(name: &Rc<str>) -> Rc<str> {
    if name.chars().all(|ch| ch.is_ascii_uppercase()) {
        name.clone()
    } else {
        Rc::<str>::from(name.to_ascii_uppercase())
    }
}

//===========================================================================//
