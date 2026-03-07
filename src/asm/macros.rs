use crate::parse::{
    AsmInvokeAst, AsmStmtAst, ExprAst, ExprAstNode, IdentifierAst, ParseError,
    ParseResult, SrcSpan, Token, TokenValue,
};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub struct MacroTable {
    definitions: HashMap<MacroSignature, Vec<MacroDefinition>>,
}

impl MacroTable {
    pub fn new() -> MacroTable {
        let mut definitions = HashMap::new();
        definitions.insert(
            MacroSignature { name: Rc::from("nop"), num_args: 0 },
            vec![MacroDefinition {
                params: vec![],
                body: vec![AsmStmtAst::U8(ExprAst {
                    span: SrcSpan::from_byte_range(0..0),
                    node: ExprAstNode::IntLiteral(BigInt::from(0x54u8)),
                })],
            }],
        );
        definitions.insert(
            MacroSignature { name: Rc::from("req"), num_args: 1 },
            vec![MacroDefinition {
                params: vec![MacroParameter::Identifier {
                    placeholder: Rc::from("%label"),
                }],
                body: vec![AsmStmtAst::Import(IdentifierAst {
                    span: SrcSpan::from_byte_range(0..0),
                    name: Rc::from("%label"),
                })],
            }],
        );
        definitions.insert(
            MacroSignature { name: Rc::from("asl"), num_args: 1 },
            vec![MacroDefinition {
                params: vec![MacroParameter::Exact {
                    pattern: vec![TokenValue::Identifier(Rc::from("a"))],
                }],
                body: vec![AsmStmtAst::U8(ExprAst {
                    span: SrcSpan::from_byte_range(0..0),
                    node: ExprAstNode::IntLiteral(BigInt::from(0x0au8)),
                })],
            }],
        );
        definitions.insert(
            MacroSignature { name: Rc::from("lda"), num_args: 1 },
            vec![MacroDefinition {
                params: vec![MacroParameter::Expression {
                    prefix: vec![TokenValue::Pound],
                    placeholder: Rc::from("%expr"),
                    suffix: vec![],
                }],
                body: vec![
                    AsmStmtAst::U8(ExprAst {
                        span: SrcSpan::from_byte_range(0..0),
                        node: ExprAstNode::IntLiteral(BigInt::from(0x45u8)),
                    }),
                    AsmStmtAst::U8(ExprAst {
                        span: SrcSpan::from_byte_range(0..0),
                        node: ExprAstNode::Identifier(Rc::from("%expr")),
                    }),
                ],
            }],
        );
        MacroTable { definitions }
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
        args: &[Vec<Token>],
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
    Expression {
        prefix: Vec<TokenValue>,
        placeholder: Rc<str>,
        suffix: Vec<TokenValue>,
    },
    Identifier {
        // TODO: allow prefix/suffix, and maybe multiple identifiers per arg
        placeholder: Rc<str>,
    },
}

impl MacroParameter {
    pub fn try_match<'a>(
        &self,
        arg: &'a [Token],
    ) -> MacroResult<MacroArgument<'a>> {
        match self {
            MacroParameter::Exact { pattern } => {
                try_match_tokens(arg, pattern)?;
                Ok(MacroArgument::Exact)
            }
            MacroParameter::Expression { prefix, placeholder, suffix } => {
                if arg.len() <= prefix.len() + suffix.len() {
                    return Err(MacroError::FailedToMatchPattern);
                }
                try_match_tokens(&arg[..prefix.len()], prefix)?;
                let suffix_start = arg.len() - suffix.len();
                try_match_tokens(&arg[suffix_start..], suffix)?;
                Ok(MacroArgument::Expression {
                    placeholder: placeholder.clone(),
                    tokens: &arg[prefix.len()..suffix_start],
                })
            }
            MacroParameter::Identifier { placeholder } => {
                if let [Token { span, value: TokenValue::Identifier(name) }] =
                    arg
                {
                    Ok(MacroArgument::Identifier {
                        placeholder: placeholder.clone(),
                        id: IdentifierAst { span: *span, name: name.clone() },
                    })
                } else {
                    Err(MacroError::FailedToMatchPattern)
                }
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

enum MacroArgument<'a> {
    Exact,
    Expression { placeholder: Rc<str>, tokens: &'a [Token] },
    Identifier { placeholder: Rc<str>, id: IdentifierAst },
}

//===========================================================================//

enum MacroSubstitution {
    Expression(ExprAst),
    Identifier(IdentifierAst),
}

impl MacroSubstitution {
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
        args: &[Vec<Token>],
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
                MacroArgument::Expression { placeholder, tokens } => {
                    match ExprAst::parse(tokens) {
                        Ok(expr) => {
                            subs.insert(
                                placeholder,
                                MacroSubstitution::Expression(expr),
                            );
                        }
                        Err(mut errs) => errors.append(&mut errs),
                    }
                }
                MacroArgument::Identifier { placeholder, id } => {
                    subs.insert(
                        placeholder,
                        MacroSubstitution::Identifier(id),
                    );
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
            | ExprAstNode::IntLiteral(_)
            | ExprAstNode::StrLiteral(_) => expression.clone(),
            ExprAstNode::Identifier(name) => {
                if let Some(substitution) = self.subs.get(name) {
                    substitution.unwrap_expression()
                } else {
                    expression.clone()
                }
            }
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
