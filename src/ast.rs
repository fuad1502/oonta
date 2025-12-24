use std::rc::Rc;

use crate::{symbol::Span, typ::Typed};

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
//     PartialExpr(
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

pub struct Ast {
    pub binds: Vec<Bind>,
}

pub struct Bind {
    pub name: Span,
    pub expr: Rc<dyn Expr>,
    pub span: Span,
}

pub trait Expr: Spanned + Typed {}

pub trait Spanned {
    fn span(&self) -> &Span;
}

pub enum LiteralExpr {
    Integer(i32, Span),
}

pub struct VarExpr {
    pub id: Span,
}

pub enum FunExpr {
    Identifier(Span),
    Anonymous(AnonymousFunExpr),
}

pub struct AnonymousFunExpr {
    pub params: Vec<Span>,
    pub body: Rc<dyn Expr>,
    pub captures: Vec<String>,
    pub span: Span,
}

pub struct ApplicationExpr {
    pub fun: Rc<FunExpr>,
    pub binds: Vec<(String, Rc<dyn Expr>)>,
    pub span: Span,
}

pub struct PartialExpr {
    pub fun: Rc<FunExpr>,
    pub binds: Vec<(String, Rc<dyn Expr>)>,
    pub span: Span,
}

pub struct LetInExpr {
    pub bind: (Span, Rc<dyn Expr>),
    pub expr: Rc<dyn Expr>,
    pub span: Span,
}

pub struct BinOpExpr {
    pub op: Operator,
    pub lhs: Rc<dyn Expr>,
    pub rhs: Rc<dyn Expr>,
    pub span: Span,
}

impl Expr for LiteralExpr {}

impl Expr for VarExpr {}

impl Expr for FunExpr {}

impl Expr for AnonymousFunExpr {}

impl Expr for ApplicationExpr {}

impl Expr for PartialExpr {}

impl Expr for LetInExpr {}

impl Expr for BinOpExpr {}

pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
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

impl Spanned for LiteralExpr {
    fn span(&self) -> &Span {
        match self {
            LiteralExpr::Integer(_, span) => span,
        }
    }
}

impl Spanned for VarExpr {
    fn span(&self) -> &Span {
        &self.id
    }
}

impl Spanned for FunExpr {
    fn span(&self) -> &Span {
        match self {
            FunExpr::Identifier(span) => span,
            FunExpr::Anonymous(anonym) => anonym.span(),
        }
    }
}

impl Spanned for AnonymousFunExpr {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for ApplicationExpr {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for PartialExpr {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for LetInExpr {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for BinOpExpr {
    fn span(&self) -> &Span {
        &self.span
    }
}
