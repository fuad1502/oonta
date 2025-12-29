use crate::{ast::Ast, ir_builder::ir::Module, typ::Context};

pub mod ir;

pub struct IRBuilder<'a> {
    ast: &'a Ast,
    context: &'a Context,
}

impl<'a> IRBuilder<'a> {
    pub fn new(ast: &Ast, context: &Context) -> Self {
        todo!()
    }

    pub fn build(self) -> Module {
        todo!()
    }
}
