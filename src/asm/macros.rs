use crate::parse::{
    AsmDefMacroAst, AsmInvokeAst, AsmMacroArgAst, AsmStmtAst, ExprAst,
    ExprAstNode, IdentifierAst, ParseError, ParseResult, SrcSpan, Token,
    TokenValue,
};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) struct MacroTable {
    definitions: HashMap<MacroSignature, Vec<MacroDefinition>>,
}

impl MacroTable {
    pub fn new() -> MacroTable {
        // TODO: pre-populate with architecture-specific instruction mnemonics
        MacroTable { definitions: HashMap::new() }
    }

    pub fn expand(
        &self,
        invoke_ast: AsmInvokeAst,
    ) -> ParseResult<Vec<AsmStmtAst>> {
        // TODO: support different macros sets for different architectures
        let signature = MacroSignature {
            name: Rc::from(invoke_ast.id.name.to_ascii_lowercase()),
            num_args: invoke_ast.args.len(),
        };
        if let Some(definitions) = self.definitions.get(&signature) {
            for definition in definitions {
                match definition.try_expand(&invoke_ast.args) {
                    Err(MacroError::FailedToMatchPattern) => continue,
                    Err(MacroError::FailedToParseArguments(errors)) => {
                        return Err(errors);
                    }
                    Ok(stmts) => return Ok(stmts),
                }
            }
            // TODO: Better error messages; maybe report the possible
            // definitions that would work for this macro.
            let message = format!("wrong syntax for `{}`", signature.name);
            Err(vec![ParseError::new(invoke_ast.id.span, message)])
        } else {
            // TODO: Better error messages; for example, report if a macro
            // name exists, but needs a differenct number of arguments, or
            // if the macro name only exists under a different
            // architecture.
            let message = format!("unknown macro `{}`", signature.name);
            Err(vec![ParseError::new(invoke_ast.id.span, message)])
        }
    }

    pub fn define(
        &mut self,
        def_macro_ast: AsmDefMacroAst,
    ) -> ParseResult<()> {
        let num_args = def_macro_ast.params.len();
        let mut builder = MacroBuilder::with_params(def_macro_ast.params);
        builder.scan_statements(&def_macro_ast.body);
        let definition = MacroDefinition {
            params: builder.build()?,
            body: def_macro_ast.body,
        };
        let signature =
            MacroSignature { name: def_macro_ast.id.name, num_args };
        self.definitions.entry(signature).or_default().push(definition);
        Ok(())
    }
}

//===========================================================================//

/// Helper type for `MacroTable::define`.
struct MacroBuilder {
    params: Vec<AsmMacroArgAst>,
    placeholders: HashMap<Rc<str>, PlaceholderKind>,
    errors: Vec<ParseError>,
}

impl MacroBuilder {
    fn with_params(params: Vec<AsmMacroArgAst>) -> MacroBuilder {
        let mut placeholders = HashMap::<Rc<str>, PlaceholderKind>::new();
        let mut errors = Vec::<ParseError>::new();
        for param in &params {
            for token in &param.tokens {
                if let TokenValue::Placeholder(name) = &token.value {
                    if placeholders.contains_key(name) {
                        // TODO: add more error details
                        let message = "repeated placeholder".to_string();
                        errors.push(ParseError::new(token.span, message));
                    } else {
                        placeholders
                            .insert(name.clone(), PlaceholderKind::default());
                    }
                }
            }
        }
        MacroBuilder { params, placeholders, errors }
    }

    fn scan_statements(&mut self, statements: &[AsmStmtAst]) {
        for stmt in statements {
            self.scan_statement(stmt);
        }
    }

    fn scan_statement(&mut self, statement: &AsmStmtAst) {
        match statement {
            AsmStmtAst::Import(id) | AsmStmtAst::Label(id) => {
                self.scan_identifier(id)
            }
            AsmStmtAst::U8(expr)
            | AsmStmtAst::U16le(expr)
            | AsmStmtAst::U24le(expr) => self.scan_expression(expr),
            other => todo!("scan_statement {other:?}"), // TODO
        }
    }

    fn scan_expression(&mut self, expression: &ExprAst) {
        match &expression.node {
            ExprAstNode::Placeholder(name) => {
                self.unify_placeholder(
                    expression.span,
                    name,
                    PlaceholderKind::Expression,
                );
            }
            ExprAstNode::BoolLiteral(_)
            | ExprAstNode::HereLabel
            | ExprAstNode::Identifier(_)
            | ExprAstNode::IntLiteral(_)
            | ExprAstNode::StrLiteral(_) => {}
            ExprAstNode::UnOp(_, subexpr) => {
                self.scan_expression(subexpr);
            }
            ExprAstNode::BinOp(_, lhs, rhs)
            | ExprAstNode::Index(_, lhs, rhs) => {
                self.scan_expression(lhs);
                self.scan_expression(rhs);
            }
            ExprAstNode::ListLiteral(items)
            | ExprAstNode::TupleLiteral(items) => {
                for item in items {
                    self.scan_expression(item);
                }
            }
        }
    }

    fn scan_identifier(&mut self, identifier: &IdentifierAst) {
        if identifier.is_placeholder {
            self.unify_placeholder(
                identifier.span,
                &identifier.name,
                PlaceholderKind::Identifier,
            );
        }
    }

    fn unify_placeholder(
        &mut self,
        span: SrcSpan,
        name: &str,
        requirement: PlaceholderKind,
    ) {
        if let Some(kind) = self.placeholders.get_mut(name) {
            kind.unify_with(requirement);
        } else {
            let message = format!("undeclared placeholder: {}", name);
            self.errors.push(ParseError::new(span, message));
        }
    }

    fn build(mut self) -> ParseResult<Vec<MacroParameter>> {
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
                        .map(|token| token.value)
                        .collect(),
                },
                [(index, name)] => {
                    let suffix = param.tokens[(index + 1)..]
                        .iter()
                        .map(|token| token.value.clone())
                        .collect();
                    let prefix = param.tokens[..index]
                        .iter()
                        .map(|token| token.value.clone())
                        .collect();
                    MacroParameter::Placeholder {
                        prefix,
                        kind: self.placeholders[name],
                        name: name.clone(),
                        suffix,
                    }
                }
                _ => {
                    // TODO better error message
                    let message = "multiple placeholders".to_string();
                    self.errors.push(ParseError::new(param.span, message));
                    continue;
                }
            });
        }
        if self.errors.is_empty() { Ok(params) } else { Err(self.errors) }
    }
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
    pub name: Rc<str>,
    pub num_args: usize,
}

//===========================================================================//

enum MacroError {
    FailedToMatchPattern,
    FailedToParseArguments(Vec<ParseError>),
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
        pattern: Vec<TokenValue>,
    },
    Placeholder {
        prefix: Vec<TokenValue>,
        kind: PlaceholderKind,
        name: Rc<str>,
        suffix: Vec<TokenValue>,
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
    pattern: &[TokenValue],
) -> MacroResult<()> {
    if tokens.len() != pattern.len() {
        return Err(MacroError::FailedToMatchPattern);
    }
    for (token, value) in tokens.iter().zip(pattern.iter()) {
        if &token.value != value {
            return Err(MacroError::FailedToMatchPattern);
        }
    }
    Ok(())
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
    ) -> ParseResult<MacroSubstitution> {
        match kind {
            PlaceholderKind::Expression => {
                let expr = ExprAst::parse(tokens)?;
                Ok(MacroSubstitution::Expression(expr))
            }
            PlaceholderKind::Identifier => {
                let id = IdentifierAst::parse(tokens)?;
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
        let mut errors = Vec::<ParseError>::new();
        for macro_arg in matched {
            match macro_arg {
                MacroArgument::Exact => {}
                MacroArgument::Placeholder { kind, name, tokens } => {
                    match MacroSubstitution::parse_tokens(kind, tokens) {
                        Ok(sub) => {
                            subs.insert(name, sub);
                        }
                        Err(mut errs) => errors.append(&mut errs),
                    }
                }
            }
        }
        if errors.is_empty() {
            Ok(MacroExpansion { subs })
        } else {
            Err(MacroError::FailedToParseArguments(errors))
        }
    }

    fn expand_statements(&self, statements: &[AsmStmtAst]) -> Vec<AsmStmtAst> {
        statements.iter().map(|stmt| self.expand_statement(stmt)).collect()
    }

    fn expand_statement(&self, statement: &AsmStmtAst) -> AsmStmtAst {
        match statement {
            AsmStmtAst::Import(id) => {
                AsmStmtAst::Import(self.expand_identifier(id))
            }
            AsmStmtAst::Label(id) => {
                AsmStmtAst::Label(self.expand_identifier(id))
            }
            AsmStmtAst::U8(expr) => {
                AsmStmtAst::U8(self.expand_expression(expr))
            }
            AsmStmtAst::U16le(expr) => {
                AsmStmtAst::U16le(self.expand_expression(expr))
            }
            AsmStmtAst::U24le(expr) => {
                AsmStmtAst::U24le(self.expand_expression(expr))
            }
            other => todo!("macro expand {other:?}"), // TODO
        }
    }

    fn expand_expressions(&self, expressions: &[ExprAst]) -> Vec<ExprAst> {
        expressions.iter().map(|expr| self.expand_expression(expr)).collect()
    }

    fn expand_expression(&self, expression: &ExprAst) -> ExprAst {
        match &expression.node {
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
