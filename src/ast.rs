mod ast_printer;

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::ast_printer::AstPrinter,
    custom_types::{CustomTypes, Variant},
    lexer::Lexer,
    symbol::Span,
};

#[derive(Default)]
pub struct Ast {
    pub binds: Vec<Bind>,
    pub custom_types: CustomTypes,
}

pub enum Stmt {
    Bind(Bind),
    TypeDecl(Variant),
}

pub struct Bind {
    pub name: Option<Span>,
    pub expr: Rc<RefCell<Expr>>,
}

pub enum Expr {
    Literal(LiteralExpr),
    Construction(ConstructionExpr),
    Var(VarExpr),
    Fun(FunExpr),
    Application(ApplicationExpr),
    LetIn(LetInExpr),
    BinOp(BinOpExpr),
    Conditional(CondExpr),
    PatternMatch(PatternMatchExpr),
    Tuple(TupleExpr),
}

pub enum LiteralExpr {
    Integer(i64, Span),
    Unit(Span),
}

pub struct VarExpr {
    pub id: Span,
}

pub struct FunExpr {
    pub params: Vec<Span>,
    pub body: Rc<RefCell<Expr>>,
    pub captures: Vec<String>,
    pub recursive_bind: Option<String>,
    pub span: Span,
}

pub struct ApplicationExpr {
    pub fun: Rc<RefCell<Expr>>,
    pub binds: Vec<Rc<RefCell<Expr>>>,
    pub span: Span,
}

pub struct ConstructionExpr {
    pub cons: String,
    pub arg: Option<Rc<RefCell<Expr>>>,
    pub span: Span,
}

pub struct LetInExpr {
    pub bind: (Span, Rc<RefCell<Expr>>),
    pub expr: Rc<RefCell<Expr>>,
    pub span: Span,
}

pub struct BinOpExpr {
    pub op: Operator,
    pub lhs: Rc<RefCell<Expr>>,
    pub rhs: Rc<RefCell<Expr>>,
    pub span: Span,
}

pub struct CondExpr {
    pub cond: Rc<RefCell<Expr>>,
    pub yes: Rc<RefCell<Expr>>,
    pub no: Rc<RefCell<Expr>>,
    pub span: Span,
}

pub struct PatternMatchExpr {
    pub matched: Rc<RefCell<Expr>>,
    pub branches: Vec<(Pattern, Rc<RefCell<Expr>>)>,
    pub span: Span,
}

pub enum Pattern {
    Tuple(Vec<Pattern>),
    Identifier(Span),
    Literal(LiteralExpr),
    None,
    // Variant Constructor
}

pub struct TupleExpr {
    pub elements: Vec<Rc<RefCell<Expr>>>,
    pub span: Span,
}

#[derive(Copy, Clone)]
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

impl From<Stmt> for Ast {
    fn from(stmt: Stmt) -> Self {
        let mut ast = Self::default();
        ast.insert_stmt(stmt);
        ast
    }
}

impl Ast {
    pub fn insert_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Bind(bind) => self.binds.push(bind),
            Stmt::TypeDecl(variant) => self.custom_types.add_variant(variant),
        }
    }
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::Literal(LiteralExpr::Integer(_, span)) => span,
            Expr::Literal(LiteralExpr::Unit(span)) => span,
            Expr::Var(VarExpr { id }) => id,
            Expr::Fun(FunExpr { span, .. }) => span,
            Expr::Application(ApplicationExpr { span, .. }) => span,
            Expr::LetIn(LetInExpr { span, .. }) => span,
            Expr::BinOp(BinOpExpr { span, .. }) => span,
            Expr::Conditional(CondExpr { span, .. }) => span,
            Expr::Tuple(TupleExpr { span, .. }) => span,
            Expr::PatternMatch(PatternMatchExpr { span, .. }) => span,
            Expr::Construction(ConstructionExpr { span, .. }) => span,
        }
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

impl Ast {
    pub fn pretty_print(&self, lexer: &Lexer) {
        let printer = AstPrinter::new(lexer);
        printer.pretty_print(self);
    }
}

impl ApplicationExpr {
    pub fn pretty_print(&self, lexer: &Lexer) {
        let mut printer = AstPrinter::new(lexer);
        printer.pretty_print_application_expr(self);
    }
}

impl Pattern {
    pub fn has_literal(&self) -> bool {
        match self {
            Pattern::Tuple(elements) => elements.iter().any(|e| e.has_literal()),
            Pattern::Identifier(_) => false,
            Pattern::Literal(_) => true,
            Pattern::None => false,
        }
    }

    pub fn has_identifier(&self) -> bool {
        match self {
            Pattern::Tuple(elements) => elements.iter().any(|e| e.has_identifier()),
            Pattern::Identifier(_) => true,
            Pattern::Literal(_) => false,
            Pattern::None => false,
        }
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Star => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Eq => write!(f, "="),
            Operator::Lte => write!(f, "<="),
            Operator::Lt => write!(f, "<"),
            Operator::Gte => write!(f, ">="),
            Operator::Gt => write!(f, ">"),
        }
    }
}
