use std::collections::HashMap;

use crate::ast::Expr;

pub enum Type {
    Primitive(Primitive),
    Fun(Vec<Box<Type>>),
    Template(usize),
    Unknown,
}

pub enum Primitive {
    Integer,
}

pub struct TypeCache {
    cache: HashMap<*const Expr, Type>,
}
