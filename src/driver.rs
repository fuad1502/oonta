use std::{
    fs::File,
    io::BufWriter,
    path::{Path, PathBuf},
    process::Command,
};

use crate::{
    application_visitor::transform_applications,
    ast::{Ast, Expr},
    ast_builder::AstBuilder,
    ir_builder::{IRBuilder, ir::Module},
    lexer::Lexer,
    parser::Parser,
    symbol::Symbol,
    typ::{self, TypeMap, TypeResolver},
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum CompileOptions {
    TopLevel,
    CreateObjFile,
    CreateExecutable,
}

pub fn compile(src_path: &Path, out_path: &Path, options: &[CompileOptions]) -> Result<(), String> {
    let mut lexer = Lexer::new(src_path).map_err(|e| e.to_string())?;
    let cst_root = parse(&mut lexer)?;
    let ast = build_ast(&lexer, &cst_root);
    let mut type_map = match resolve_types(&lexer, &ast) {
        Ok(type_map) => type_map,
        Err(e) => return Err(e.report(&lexer)),
    };
    transform_applications(&ast, &mut type_map);
    print_global_types(&ast, &type_map, &lexer);
    let is_create_executable = options.contains(&CompileOptions::CreateExecutable);
    let is_create_objfile = options.contains(&CompileOptions::CreateObjFile);
    let is_top_level = options.contains(&CompileOptions::TopLevel);
    let is_top_level = is_top_level || is_create_executable;
    let module = build_module(&ast, &type_map, &lexer, is_top_level);
    write_module_to_file(&module, out_path).map_err(|e| e.to_string())?;
    if is_create_objfile || is_create_executable {
        let obj_file = create_obj_file(out_path)?;
        if is_create_executable {
            let _ = create_executable(&obj_file)?;
        }
    }
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

fn resolve_types(lexer: &Lexer, ast: &Ast) -> Result<TypeMap, typ::Error> {
    let type_resolver = TypeResolver::new(lexer);
    type_resolver.resolve_types(ast)
}

fn print_global_types(ast: &Ast, type_map: &TypeMap, lexer: &Lexer) {
    for binding in &ast.binds {
        if let Some(name) = &binding.name {
            let name = lexer.str_from_span(name);
            let typ = type_map
                .get(&*binding.expr.borrow() as *const Expr)
                .unwrap();
            println!("{name}: {}", typ.borrow());
        }
    }
}

fn build_module(ast: &Ast, type_map: &TypeMap, lexer: &Lexer, is_top_level: bool) -> Module {
    let ir_builder = IRBuilder::new(type_map, lexer, is_top_level);
    ir_builder.build(ast)
}

fn write_module_to_file(module: &Module, path: &Path) -> std::io::Result<()> {
    let file = File::create(path)?;
    let wr = BufWriter::new(file);
    module.serialize(Box::new(wr))
}

fn create_obj_file(path: &Path) -> Result<PathBuf, String> {
    let mut cmd = Command::new("llc");
    let obj_file = path.with_extension("o");
    cmd.args([
        "-relocation-model=pic",
        "--filetype=obj",
        "-o",
        obj_file.to_str().unwrap(),
        path.to_str().unwrap(),
    ]);
    execute_command(cmd)?;
    Ok(obj_file)
}

fn create_executable(path: &Path) -> Result<PathBuf, String> {
    let mut cmd = Command::new("clang");
    let executable = path.with_extension("out");
    cmd.args(["-o", executable.to_str().unwrap(), path.to_str().unwrap()]);
    execute_command(cmd)?;
    Ok(executable)
}

fn execute_command(mut cmd: Command) -> Result<(), String> {
    let error_message = format!("Error: failed to execute command ({cmd:?})");
    let output = cmd.output().map_err(|e| format!("{error_message}: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "{error_message}:\nStdout:\n{}Stderr:\n{}",
            str::from_utf8(&output.stdout).unwrap(),
            str::from_utf8(&output.stderr).unwrap()
        ));
    }
    Ok(())
}
