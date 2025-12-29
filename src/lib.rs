mod ast;
mod ast_builder;
mod ir_builder;
#[allow(unused)]
mod lexer;
mod parser;
#[allow(unused)]
mod symbol;
mod typ;

use std::path::Path;

use crate::{
    ast::Ast,
    ast_builder::AstBuilder,
    ir_builder::{IRBuilder, ir::Module},
    lexer::Lexer,
    parser::Parser,
    symbol::Symbol,
    typ::{Context, TypeResolver},
};

pub fn compile(file_path: &Path) -> Result<(), String> {
    let mut lexer = Lexer::new(file_path).map_err(|e| e.to_string())?;
    let cst_root = parse(&mut lexer)?;
    let ast = build_ast(&lexer, &cst_root);
    let ctx = resolve_types(&lexer, &ast)?;
    println!("{ctx}");
    let _ = build_module(&ast, &ctx);
    Ok(())
}

fn parse(lexer: &mut Lexer) -> Result<Symbol, String> {
    let mut parser = Parser::new();
    parser.parse(lexer)
}

fn build_ast(lexer: &Lexer, cst_root: &Symbol) -> Ast {
    let mut ast_builder = AstBuilder::new(lexer);
    ast_builder.visit(cst_root)
}

fn resolve_types(lexer: &Lexer, ast: &Ast) -> Result<Context, String> {
    let type_resolver = TypeResolver::new(lexer);
    type_resolver.resolve_types(ast)
}

fn build_module(ast: &Ast, context: &Context) -> Module {
    let ir_builder = IRBuilder::new(ast, context);
    ir_builder.build()
}
