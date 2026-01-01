mod application_visitor;
mod ast;
mod ast_builder;
mod ir_builder;
#[allow(unused)]
mod lexer;
mod parser;
#[allow(unused)]
mod symbol;
mod typ;

use std::{fs::File, io::BufWriter, path::Path};

use crate::{
    application_visitor::transform_applications,
    ast::{Ast, Expr},
    ast_builder::AstBuilder,
    ir_builder::{IRBuilder, ir::Module},
    lexer::Lexer,
    parser::Parser,
    symbol::Symbol,
    typ::{TypeMap, TypeResolver},
};

pub fn compile(src_path: &Path, out_path: &Path) -> Result<(), String> {
    let mut lexer = Lexer::new(src_path).map_err(|e| e.to_string())?;
    let cst_root = parse(&mut lexer)?;
    let ast = build_ast(&lexer, &cst_root);
    let mut type_map = resolve_types(&lexer, &ast)?;
    transform_applications(&ast, &mut type_map);
    print_global_types(&ast, &type_map, &lexer);
    let module = build_module(&ast, &type_map, &lexer);
    write_module_to_file(&module, out_path).map_err(|e| e.to_string())?;
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

fn resolve_types(lexer: &Lexer, ast: &Ast) -> Result<TypeMap, String> {
    let type_resolver = TypeResolver::new(lexer);
    type_resolver.resolve_types(ast)
}

fn print_global_types(ast: &Ast, type_map: &TypeMap, lexer: &Lexer) {
    for binding in &ast.binds {
        let name = lexer.str_from_span(&binding.name);
        let typ = type_map
            .get(&*binding.expr.borrow() as *const Expr)
            .unwrap();
        println!("{name}: {}", typ.borrow());
    }
}

fn build_module(ast: &Ast, type_map: &TypeMap, lexer: &Lexer) -> Module {
    let ir_builder = IRBuilder::new(type_map, lexer);
    ir_builder.build(ast)
}

fn write_module_to_file(module: &Module, path: &Path) -> std::io::Result<()> {
    let file = File::create(path)?;
    let wr = BufWriter::new(file);
    module.serialize(Box::new(wr))
}
