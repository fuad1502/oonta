use std::{cell::RefCell, rc::Rc};

use crate::symbol::Span;

// let x = 5
// Bind("x", LiteralExpr(5))
//
// let add a b = a + b + x
// Bind("add",
//     FunExpr::Anonymous(
//         ["a", "b"],
//         BinOpExpr("+", BinOpExpr("+", VarExpr("a"), VarExpr("b")), "x")
//     )
// )
//
// let addthree = add 3
// Bind("addthree",
//     ApplicationExpr(
//         FunExpr::Identifier("add"),
//         [("a", LiteralExpr(3))]
//     )
// )
//
// let y = add x x
// Bind("y",
//     ApplicationExpr(
//         FunExpr::Identifier("add"),
//         [("a", VarExpr("x")), ("b", VarExpr("x"))]
//     )
// )

#[derive(Debug)]
pub struct Ast {
    pub binds: Vec<Bind>,
}

#[derive(Debug)]
pub struct Bind {
    pub name: Span,
    pub expr: Rc<RefCell<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(LiteralExpr),
    Var(VarExpr),
    Fun(FunExpr),
    Application(ApplicationExpr),
    LetIn(LetInExpr),
    BinOp(BinOpExpr),
}

#[derive(Debug, Clone)]
pub enum LiteralExpr {
    Integer(i32, Span),
}

#[derive(Debug, Clone)]
pub struct VarExpr {
    pub id: Span,
}

#[derive(Debug, Clone)]
pub struct FunExpr {
    pub params: Vec<Span>,
    pub body: Rc<RefCell<Expr>>,
    pub captures: Vec<String>,
    pub recursive_bind: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ApplicationExpr {
    pub fun: Rc<RefCell<Expr>>,
    pub binds: Vec<Rc<RefCell<Expr>>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LetInExpr {
    pub bind: (Span, Rc<RefCell<Expr>>),
    pub expr: Rc<RefCell<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinOpExpr {
    pub op: Operator,
    pub lhs: Rc<RefCell<Expr>>,
    pub rhs: Rc<RefCell<Expr>>,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Lte,
    Lt,
    Gte,
    Gt,
}

impl From<Bind> for Ast {
    fn from(bind: Bind) -> Self {
        Self { binds: vec![bind] }
    }
}

impl Ast {
    pub fn append(&mut self, bind: Bind) {
        self.binds.push(bind);
    }
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::Literal(LiteralExpr::Integer(_, span)) => span,
            Expr::Var(VarExpr { id }) => id,
            Expr::Fun(FunExpr { span, .. }) => span,
            Expr::Application(ApplicationExpr { span, .. }) => span,
            Expr::LetIn(LetInExpr { span, .. }) => span,
            Expr::BinOp(BinOpExpr { span, .. }) => span,
        }
    }

    pub fn integer(value: i32, span: Span) -> Self {
        Self::Literal(LiteralExpr::Integer(value, span.clone()))
    }

    pub fn var(span: Span) -> Self {
        Self::Var(VarExpr { id: span })
    }

    pub fn fun(
        params: Vec<Span>,
        body: Rc<RefCell<Expr>>,
        captures: Vec<String>,
        recursive_bind: Option<String>,
        span: Span,
    ) -> Self {
        Self::Fun(FunExpr {
            params,
            body,
            captures,
            recursive_bind,
            span,
        })
    }

    pub fn binop(op: Operator, lhs: Rc<RefCell<Expr>>, rhs: Rc<RefCell<Expr>>, span: Span) -> Self {
        Self::BinOp(BinOpExpr { op, lhs, rhs, span })
    }
}
