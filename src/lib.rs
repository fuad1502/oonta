mod ast;
mod ast_builder;
#[allow(unused)]
mod lexer;
mod parser;
mod symbol;
mod typ;

use std::path::Path;

use crate::{ast_builder::build_ast, lexer::Lexer, parser::Parser, symbol::Symbol};

pub fn compile(file_path: &Path) -> Result<(), String> {
    let mut lexer = Lexer::new(file_path).map_err(|e| e.to_string())?;
    let cst_root = parse(&mut lexer)?;
    cst_root.pretty_print(&lexer, 0);
    let _ = build_ast(cst_root);
    Ok(())
}

fn parse(lexer: &mut Lexer) -> Result<Symbol, String> {
    let mut parser = Parser::new();
    parser.parse(lexer)
}
