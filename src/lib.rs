mod ast;
mod ast_builder;
#[allow(unused)]
mod lexer;
mod parser;
mod symbol;
mod typ;

use std::path::Path;

use crate::{
    ast::Ast,
    ast_builder::AstBuilder,
    lexer::Lexer,
    parser::Parser,
    symbol::Symbol,
    typ::{Context, TypeResolver},
};

pub fn compile(file_path: &Path) -> Result<(), String> {
    let mut lexer = Lexer::new(file_path).map_err(|e| e.to_string())?;
    let cst_root = parse(&mut lexer)?;
    cst_root.pretty_print(&lexer, 0);
    let ast = build_ast(&lexer, &cst_root)?;
    let ctx = resolve_types(&lexer, ast)?;
    dbg!(ctx);
    Ok(())
}

fn parse(lexer: &mut Lexer) -> Result<Symbol, String> {
    let mut parser = Parser::new();
    parser.parse(lexer)
}

fn build_ast(lexer: &Lexer, cst_root: &Symbol) -> Result<Ast, String> {
    let mut ast_builder = AstBuilder::new(&lexer);
    ast_builder.visit(cst_root)
}

fn resolve_types(lexer: &Lexer, ast: Ast) -> Result<Context, String> {
    let type_resolver = TypeResolver::new(lexer);
    type_resolver.resolve_types(ast)
}
