use std::{
    fs::File,
    io::BufWriter,
    path::{Path, PathBuf},
    process::Command,
    time::Instant,
};

use crate::{
    application_visitor::transform_applications,
    ast::{Ast, Expr},
    ast_builder::AstBuilder,
    custom_types::CustomTypes,
    ir_builder::{IRBuilder, ir::Module},
    lexer::Lexer,
    parser::Parser,
    symbol::Symbol,
    terminal_colors::{BLUE, END, GREEN, RED, YELLOW},
    typ::{self, TypeMap, TypeResolver},
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum CompileOptions {
    TopLevel,
    CreateObjFile,
    CreateExecutable,
    DebugPhases,
}

struct Driver {
    debug_phases: bool,
    top_level: bool,
    create_obj_file: bool,
    create_executable: bool,
    step: &'static str,
    start_time: Instant,
}

pub fn compile(src_path: &Path, out_path: &Path, options: &[CompileOptions]) -> Result<(), String> {
    let debug_phases = options.contains(&CompileOptions::DebugPhases);
    let create_executable = options.contains(&CompileOptions::CreateExecutable);
    let create_obj_file = options.contains(&CompileOptions::CreateObjFile);
    let top_level = options.contains(&CompileOptions::TopLevel);

    let top_level = top_level || create_executable;
    let create_obj_file = create_obj_file || create_executable;

    Driver {
        debug_phases,
        top_level,
        create_obj_file,
        create_executable,
        step: "",
        start_time: Instant::now(),
    }
    .compile(src_path, out_path)
}

impl Driver {
    fn compile(mut self, src_path: &Path, out_path: &Path) -> Result<(), String> {
        self.dbg_start("Lexing & Parsing");
        let mut lexer = Lexer::new(src_path).map_err(|e| {
            format!(
                "{RED}Error{END}: unable to open input file \"{}\": {e}",
                src_path.to_str().unwrap()
            )
        })?;
        let cst_root = parse(&mut lexer)?;
        self.dbg_end();

        self.dbg_start("Build AST");
        let (ast, custom_types) = build_ast(&lexer, &cst_root);
        if self.debug_phases {
            ast.pretty_print(&lexer);
        }
        self.dbg_end();

        self.dbg_start("Resolve types");
        let mut type_map = match resolve_types(&lexer, &ast, &custom_types) {
            Ok(type_map) => type_map,
            Err(e) => return Err(e.report(&lexer)),
        };
        if self.debug_phases {
            print_global_types(&ast, &type_map, &lexer);
        }
        self.dbg_end();

        self.dbg_start("Transform application expressions");
        transform_applications(&ast, &mut type_map, &lexer, self.debug_phases);
        self.dbg_end();

        self.dbg_start("Build LLVM module");
        let module = build_module(&ast, &type_map, &lexer, self.top_level);
        self.dbg_end();

        self.dbg_start("Write LLVM module");
        write_module_to_file(&module, out_path).map_err(|e| e.to_string())?;
        self.dbg_end();

        if self.create_obj_file {
            self.dbg_start("LLVM backend");
            let obj_file = create_obj_file(out_path)?;
            if self.create_executable {
                let _ = create_executable(&obj_file)?;
            }
            self.dbg_end();
        }
        Ok(())
    }

    fn dbg_start(&mut self, step: &'static str) {
        if self.debug_phases {
            self.step = step;
            self.start_time = Instant::now();
            println!("{GREEN}=> {step} Start{END}");
        }
    }

    fn dbg_end(&mut self) {
        if self.debug_phases {
            let duration = self.start_time.elapsed();
            println!(
                "{GREEN}=> {} End ({} ms){END}",
                self.step,
                duration.as_millis()
            );
            self.step = "";
        }
    }
}

fn parse(lexer: &mut Lexer) -> Result<Symbol, String> {
    let mut parser = Parser::new();
    parser.parse(lexer)
}

fn build_ast(lexer: &Lexer, cst_root: &Symbol) -> (Ast, CustomTypes) {
    let ast_builder = AstBuilder::new(lexer);
    ast_builder.build(cst_root)
}

fn resolve_types(
    lexer: &Lexer,
    ast: &Ast,
    custom_types: &CustomTypes,
) -> Result<TypeMap, typ::Error> {
    let type_resolver = TypeResolver::new(custom_types, lexer);
    type_resolver.resolve_types(ast)
}

fn print_global_types(ast: &Ast, type_map: &TypeMap, lexer: &Lexer) {
    println!("Top level bindings:");
    for binding in &ast.binds {
        if let Some(name) = &binding.name {
            let name = lexer.str_from_span(name);
            let typ = type_map
                .get(&*binding.expr.borrow() as *const Expr)
                .unwrap();
            println!("{YELLOW}{name}{END}: {BLUE}{}{END}", typ.borrow());
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

#[cfg(test)]
mod test {
    use core::convert::From;
    use std::{
        path::{Path, PathBuf},
        process::Command,
    };

    use crate::driver::{CompileOptions, compile};

    #[test]
    fn ll() {
        let options = vec![];
        let out_path = out_path("ll");
        clear_output_files(&out_path);
        compile(&src_path(), &out_path, &options).unwrap();

        assert!(std::fs::exists(&out_path).unwrap());
        assert!(!std::fs::exists(out_path.with_extension("o")).unwrap());
        assert!(!std::fs::exists(out_path.with_extension("out")).unwrap());
        clear_output_files(&out_path);
    }

    #[test]
    fn obj() {
        let options = vec![CompileOptions::CreateObjFile];
        let out_path = out_path("obj");
        clear_output_files(&out_path);
        compile(&src_path(), &out_path, &options).unwrap();

        assert!(std::fs::exists(&out_path).unwrap());
        assert!(std::fs::exists(out_path.with_extension("o")).unwrap());
        assert!(!std::fs::exists(out_path.with_extension("out")).unwrap());
        clear_output_files(&out_path);
    }

    #[test]
    fn exec() {
        let options = vec![CompileOptions::CreateExecutable];
        let out_path = out_path("exec");
        clear_output_files(&out_path);
        compile(&src_path(), &out_path, &options).unwrap();

        assert!(std::fs::exists(&out_path).unwrap());
        assert!(std::fs::exists(out_path.with_extension("o")).unwrap());
        assert!(std::fs::exists(out_path.with_extension("out")).unwrap());

        let mut cmd = Command::new(out_path.with_extension("out"));
        let output = cmd.output().unwrap();
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert_eq!(stdout, "9\n120\n3\n");
        clear_output_files(&out_path);
    }

    fn src_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("ocaml")
            .join("test.ml")
    }

    fn out_path(postfix: &str) -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("ocaml")
            .join(format!("test-{postfix}.ll"))
    }

    fn clear_output_files(out_path: &Path) {
        let _ = std::fs::remove_file(out_path);
        let _ = std::fs::remove_file(out_path.with_extension("o"));
        let _ = std::fs::remove_file(out_path.with_extension("out"));
    }
}
