use crate::error::SrcSpan;
use crate::expr::ExprFunc;
use crate::parse::{
    AsmIntDataAst, AsmIntTypeAst, AsmStmtAst, BinOpAst, ExprAst, ExprAstNode,
    IdentifierAst, IdentifierKind, Token, TokenValue,
};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) struct RcPool {
    strings: HashMap<&'static str, Rc<str>>,
}

impl RcPool {
    pub fn new() -> Self {
        Self { strings: HashMap::new() }
    }

    pub fn string(&mut self, string: &'static str) -> Rc<str> {
        self.strings.entry(string).or_insert_with(|| Rc::from(string)).clone()
    }

    pub fn apply_expr(&mut self, func: ExprFunc, arg: ExprAst) -> ExprAst {
        ExprAst {
            span: SrcSpan::INTERNAL,
            node: ExprAstNode::Apply(
                Box::new(ExprAst {
                    span: SrcSpan::INTERNAL,
                    node: ExprAstNode::Identifier(self.string(func.name())),
                }),
                Box::new(arg),
            ),
        }
    }

    pub fn binop_expr(
        &mut self,
        op: BinOpAst,
        lhs: ExprAst,
        rhs: ExprAst,
    ) -> ExprAst {
        ExprAst {
            span: SrcSpan::INTERNAL,
            node: ExprAstNode::BinOp(
                (SrcSpan::INTERNAL, op),
                Box::new(lhs),
                Box::new(rhs),
            ),
        }
    }

    pub fn constant_u8(&mut self, value: u8) -> AsmStmtAst {
        let expr = self.int_literal_expr(value);
        self.int_data_stmt(AsmIntTypeAst::U8, expr)
    }

    pub fn error_expr(&mut self, message: &'static str) -> ExprAst {
        let expr = self.str_literal_expr(message);
        self.apply_expr(ExprFunc::Error, expr)
    }

    pub fn here_label_expr(&mut self) -> ExprAst {
        ExprAst { span: SrcSpan::INTERNAL, node: ExprAstNode::HereLabel }
    }

    pub fn high_page_addr(&mut self, placeholder: &'static str) -> AsmStmtAst {
        // TODO: error unless address in [0xff00, 0xffff]
        let lhs = self.placeholder_expr(placeholder);
        let rhs = self.int_literal_expr(0xff);
        let expr = self.binop_expr(BinOpAst::BitAnd, lhs, rhs);
        self.int_data_stmt(AsmIntTypeAst::U8, expr)
    }

    pub fn identifier_token(&mut self, name: &'static str) -> Token {
        Token {
            span: SrcSpan::INTERNAL,
            value: TokenValue::Identifier(self.string(name)),
        }
    }

    pub fn int_data_stmt(
        &mut self,
        int_type: AsmIntTypeAst,
        expr: ExprAst,
    ) -> AsmStmtAst {
        AsmStmtAst::IntData(AsmIntDataAst {
            directive_span: SrcSpan::INTERNAL,
            int_type,
            expressions: vec![expr],
        })
    }

    pub fn int_literal_expr(&mut self, value: u8) -> ExprAst {
        ExprAst {
            span: SrcSpan::INTERNAL,
            node: ExprAstNode::IntLiteral(BigInt::from(value)),
        }
    }

    pub fn placeholder_expr(&mut self, placeholder: &'static str) -> ExprAst {
        ExprAst {
            span: SrcSpan::INTERNAL,
            node: ExprAstNode::Placeholder(self.string(placeholder)),
        }
    }

    pub fn placeholder_token(&mut self, placeholder: &'static str) -> Token {
        Token {
            span: SrcSpan::INTERNAL,
            value: TokenValue::Placeholder(self.string(placeholder)),
        }
    }

    pub fn placeholder_u8(&mut self, placeholder: &'static str) -> AsmStmtAst {
        let expr = self.placeholder_expr(placeholder);
        self.int_data_stmt(AsmIntTypeAst::U8, expr)
    }

    pub fn placeholder_u16le(
        &mut self,
        placeholder: &'static str,
    ) -> AsmStmtAst {
        let expr = self.placeholder_expr(placeholder);
        self.int_data_stmt(AsmIntTypeAst::U16le, expr)
    }

    pub fn placeholder_u24le(
        &mut self,
        placeholder: &'static str,
    ) -> AsmStmtAst {
        let expr = self.placeholder_expr(placeholder);
        self.int_data_stmt(AsmIntTypeAst::U24le, expr)
    }

    pub fn relative_addr(&mut self, placeholder: &'static str) -> AsmStmtAst {
        let expr = {
            let lhs = {
                let lhs = self.placeholder_expr(placeholder);
                let rhs = {
                    let lhs = self.here_label_expr();
                    let rhs = self.int_literal_expr(1);
                    self.binop_expr(BinOpAst::Add, lhs, rhs)
                };
                self.binop_expr(BinOpAst::Sub, lhs, rhs)
            };
            let rhs = self.int_literal_expr(0xff);
            // TODO: use .S8 instead of .U8 & $ff
            self.binop_expr(BinOpAst::BitAnd, lhs, rhs)
        };
        self.int_data_stmt(AsmIntTypeAst::U8, expr)
    }

    pub fn standard_id(&mut self, name: &'static str) -> IdentifierAst {
        IdentifierAst {
            span: SrcSpan::INTERNAL,
            name: self.string(name),
            kind: IdentifierKind::Standard,
        }
    }

    pub fn str_literal_expr(&mut self, value: &'static str) -> ExprAst {
        ExprAst {
            span: SrcSpan::INTERNAL,
            node: ExprAstNode::StrLiteral(self.string(value)),
        }
    }

    pub fn ternary_expr(
        &mut self,
        pred_expr: ExprAst,
        then_expr: ExprAst,
        else_expr: ExprAst,
    ) -> ExprAst {
        ExprAst {
            span: SrcSpan::INTERNAL,
            node: ExprAstNode::Conditional(
                Box::new(pred_expr),
                Box::new(then_expr),
                Box::new(else_expr),
            ),
        }
    }
}

//===========================================================================//
