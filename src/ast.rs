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

#[derive(Debug)]
pub struct Ast {
    pub binds: Vec<Bind>,
}

#[derive(Debug)]
pub struct Bind {
    pub name: Span,
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Expr {
    LiteralExpr(LiteralExpr),
    VarExpr(VarExpr),
    FunExpr(FunExpr),
    ApplicationExpr(ApplicationExpr),
    PartialExpr(PartialExpr),
    LetInExpr(LetInExpr),
    BinOpExpr(BinOpExpr),
}

#[derive(Debug)]
pub enum LiteralExpr {
    Integer(i32, Span),
}

#[derive(Debug)]
pub struct VarExpr {
    pub id: Span,
}

#[derive(Debug)]
pub enum FunExpr {
    Identifier(Span),
    Anonymous(AnonymousFunExpr),
}

#[derive(Debug)]
pub struct AnonymousFunExpr {
    pub params: Vec<Span>,
    pub body: Box<Expr>,
    pub captures: Vec<String>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ApplicationExpr {
    pub fun: Box<FunExpr>,
    pub binds: Vec<(String, Box<Expr>)>,
    pub span: Span,
}

#[derive(Debug)]
pub struct PartialExpr {
    pub fun: Box<FunExpr>,
    pub binds: Vec<(String, Box<Expr>)>,
    pub span: Span,
}

#[derive(Debug)]
pub struct LetInExpr {
    pub bind: (Span, Box<Expr>),
    pub expr: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct BinOpExpr {
    pub op: Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
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

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::LiteralExpr(LiteralExpr::Integer(_, span)) => span,
            Expr::VarExpr(VarExpr { id }) => id,
            Expr::FunExpr(FunExpr::Identifier(span)) => span,
            Expr::FunExpr(FunExpr::Anonymous(AnonymousFunExpr { span, .. })) => span,
            Expr::ApplicationExpr(ApplicationExpr { span, .. }) => span,
            Expr::PartialExpr(PartialExpr { span, .. }) => span,
            Expr::LetInExpr(LetInExpr { span, .. }) => span,
            Expr::BinOpExpr(BinOpExpr { span, .. }) => span,
        }
    }

    pub fn integer(value: i32, span: Span) -> Self {
        Self::LiteralExpr(LiteralExpr::Integer(value, span.clone()))
    }

    pub fn var(span: Span) -> Self {
        Self::VarExpr(VarExpr { id: span })
    }

    pub fn anonymous_fun(
        params: Vec<Span>,
        body: Box<Expr>,
        captures: Vec<String>,
        span: Span,
    ) -> Self {
        Self::FunExpr(FunExpr::Anonymous(AnonymousFunExpr {
            params,
            body,
            captures,
            span,
        }))
    }

    pub fn binop(op: Operator, lhs: Box<Expr>, rhs: Box<Expr>, span: Span) -> Self {
        Self::BinOpExpr(BinOpExpr { op, lhs, rhs, span })
    }
}
