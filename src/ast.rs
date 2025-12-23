use std::rc::Rc;

use crate::typ::Typed;

// let x = 5
// Bind("x", LiteralExpr(5))
//
// let add a b = a + b + x
// Bind("add",
//     FunExpr::Anynoymous(
//         ["a", "b"],
//         BinOpExpr("+", BinOpExpr("+", VarExpr("a"), VarExpr("b")), "x")
//     )
// )
//
// let y = add x x
// Bind("y",
//     ApplicationExpr(
//         FunExpr::Partial(
//             ["b"],
//             ApplicationExpr(
//                 FunExpr::Identifier("add"),
//                 ("a", VarExpr("x"))
//             ),
//         ),
//         ("b", VarExpr("x"))
//     )
// )

pub struct Ast {
    pub binds: Vec<Bind>,
}

pub struct Bind {
    pub name: String,
    pub expr: Rc<dyn Expr>,
}

pub trait Expr: Typed {}

pub enum LiteralExpr {
    Integer(i32),
}

pub struct VarExpr {
    pub id: String,
}

pub enum FunExpr {
    Identifier(String),
    Anonymous(Vec<String>, Vec<String>, Rc<dyn Expr>),
}

pub struct ApplicationExpr {
    pub fun: Rc<FunExpr>,
    pub binds: Vec<(String, Rc<dyn Expr>)>,
}

pub struct BinOpExpr {
    pub op: Operator,
    pub lhs: Rc<dyn Expr>,
    pub rhs: Rc<dyn Expr>,
}

impl Expr for LiteralExpr {}

impl Expr for VarExpr {}

impl Expr for FunExpr {}

impl Expr for ApplicationExpr {}

impl Expr for BinOpExpr {}

pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
}
