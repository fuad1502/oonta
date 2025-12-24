use std::collections::HashMap;

use crate::ast::{
    AnonymousFunExpr, ApplicationExpr, BinOpExpr, Expr, FunExpr, LetInExpr, LiteralExpr,
    PartialExpr, VarExpr,
};

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
    cache: HashMap<*const dyn Expr, Type>,
}

pub trait Typed {
    fn typ(&self, cache: &mut TypeCache) -> Type;
}

impl Typed for LiteralExpr {
    fn typ(&self, cache: &mut TypeCache) -> Type {
        todo!()
    }
}

impl Typed for VarExpr {
    fn typ(&self, cache: &mut TypeCache) -> Type {
        todo!()
    }
}

impl Typed for FunExpr {
    fn typ(&self, cache: &mut TypeCache) -> Type {
        todo!()
    }
}

impl Typed for AnonymousFunExpr {
    fn typ(&self, cache: &mut TypeCache) -> Type {
        todo!()
    }
}

impl Typed for ApplicationExpr {
    fn typ(&self, cache: &mut TypeCache) -> Type {
        todo!()
    }
}

impl Typed for PartialExpr {
    fn typ(&self, cache: &mut TypeCache) -> Type {
        todo!()
    }
}

impl Typed for LetInExpr {
    fn typ(&self, cache: &mut TypeCache) -> Type {
        todo!()
    }
}

impl Typed for BinOpExpr {
    fn typ(&self, cache: &mut TypeCache) -> Type {
        todo!()
    }
}
