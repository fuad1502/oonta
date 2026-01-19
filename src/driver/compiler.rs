use std::fs::File;
use std::io::BufWriter;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use crate::ast::{Ast, Expr};
use crate::driver::terminal_colors::{BLUE, END, GREEN, RED, YELLOW};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::pass::build_ast::AstBuilder;
use crate::pass::currying::transform_applications;
use crate::pass::ir_generation::IRBuilder;
use crate::pass::ir_generation::ir::Module;
use crate::pass::monomorphization::{MonoBinds, monomorphize};
use crate::pass::type_inference::{self, TypeMap, TypeResolver};
use crate::symbol::Symbol;
use crate::typ::custom_types::CustomTypes;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum CompileOptions {
    TopLevel,
    OptimizeIR,
    CreateObjFile,
    CreateExecutable,
    DebugPhases,
}

struct Compiler {
    debug_phases: bool,
    top_level: bool,
    optimize_ir: bool,
    create_obj_file: bool,
    create_executable: bool,
    step: &'static str,
    start_time: Instant,
}

pub fn compile(src_path: &Path, out_path: &Path, options: &[CompileOptions]) -> Result<(), String> {
    let debug_phases = options.contains(&CompileOptions::DebugPhases);
    let optimize_ir = options.contains(&CompileOptions::OptimizeIR);
    let create_executable = options.contains(&CompileOptions::CreateExecutable);
    let create_obj_file = options.contains(&CompileOptions::CreateObjFile);
    let top_level = options.contains(&CompileOptions::TopLevel);

    let top_level = top_level || create_executable;
    let create_obj_file = create_obj_file || create_executable;

    Compiler {
        debug_phases,
        top_level,
        optimize_ir,
        create_obj_file,
        create_executable,
        step: "",
        start_time: Instant::now(),
    }
    .compile(src_path, out_path)
}

impl Compiler {
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

        self.dbg_start("Monomorphization");
        let mono_binds = monomorphize(&ast, &mut type_map, &lexer, self.debug_phases);
        self.dbg_end();

        self.dbg_start("Build LLVM module");
        let module = build_module(
            &ast,
            &mono_binds,
            &type_map,
            &custom_types,
            &lexer,
            self.top_level,
        );
        self.dbg_end();

        self.dbg_start("Write LLVM module");
        write_module_to_file(&module, out_path).map_err(|e| e.to_string())?;
        self.dbg_end();

        if self.optimize_ir {
            self.dbg_start("Optimize LLVM IR");
            optimize_llvm_ir(out_path)?;
            self.dbg_end();
        }

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
) -> Result<TypeMap, type_inference::Error> {
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

fn build_module(
    ast: &Ast,
    mono_binds: &MonoBinds,
    type_map: &TypeMap,
    custom_types: &CustomTypes,
    lexer: &Lexer,
    is_top_level: bool,
) -> Module {
    let ir_builder = IRBuilder::new(type_map, custom_types, lexer, is_top_level);
    ir_builder.build(ast, mono_binds)
}

fn write_module_to_file(module: &Module, path: &Path) -> std::io::Result<()> {
    let file = File::create(path)?;
    let wr = BufWriter::new(file);
    module.serialize(Box::new(wr))
}

fn optimize_llvm_ir(path: &Path) -> Result<(), String> {
    let mut cmd = Command::new("opt");
    cmd.args([
        "-S",
        "-O3",
        "-o",
        path.to_str().unwrap(),
        path.to_str().unwrap(),
    ]);
    execute_command(cmd)?;
    Ok(())
}

fn create_obj_file(path: &Path) -> Result<PathBuf, String> {
    let mut cmd = Command::new("llc");
    let obj_file = path.with_extension("o");
    cmd.args([
        "-O3",
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
    cmd.args([
        "-o",
        executable.to_str().unwrap(),
        path.to_str().unwrap(),
        "-loonta_runtime",
    ]);
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

    use crate::driver::compiler::{CompileOptions, compile};

    #[test]
    fn ll_arithmetic() {
        ll("arithmetic", false);
    }

    #[test]
    fn ll_opt_arithmetic() {
        ll("arithmetic", true);
    }

    #[test]
    fn obj_arithmetic() {
        obj("arithmetic");
    }

    #[test]
    fn exec_arithmetic() {
        exec("arithmetic", "9120");
    }

    #[test]
    fn ll_merge_sort() {
        ll("merge_sort", false);
    }

    #[test]
    fn ll_opt_merge_sort() {
        ll("merge_sort", true);
    }

    #[test]
    fn obj_merge_sort() {
        obj("merge_sort");
    }

    #[test]
    fn exec_merge_sort() {
        exec("merge_sort", "12345");
    }

    #[test]
    fn ll_polymorphic() {
        ll("polymorphic", false);
    }

    #[test]
    fn ll_opt_polymorphic() {
        ll("polymorphic", true);
    }

    #[test]
    fn obj_polymorphic() {
        obj("polymorphic");
    }

    #[test]
    fn exec_polymorphic() {
        exec("polymorphic", "2");
    }

    #[test]
    fn ll_comparison() {
        ll("comparison", false);
    }

    #[test]
    fn ll_opt_comparison() {
        ll("comparison", true);
    }

    #[test]
    fn obj_comparison() {
        obj("comparison");
    }

    #[test]
    fn exec_comparison() {
        exec("comparison", "1100010111100110001011");
    }

    #[test]
    fn ll_parametric_variant() {
        ll("parametric_variant", false);
    }

    #[test]
    fn ll_opt_parametric_variant() {
        ll("parametric_variant", true);
    }

    #[test]
    fn obj_parametric_variant() {
        obj("parametric_variant");
    }

    #[test]
    fn exec_parametric_variant() {
        exec("parametric_variant", "1210");
    }

    #[test]
    fn ll_unit_list() {
        ll("unit_list", false);
    }

    #[test]
    fn ll_opt_unit_list() {
        ll("unit_list", true);
    }

    #[test]
    fn obj_unit_list() {
        obj("unit_list");
    }

    #[test]
    fn exec_unit_list() {
        exec("unit_list", "311000");
    }

    fn ll(test_name: &str, opt: bool) {
        let (options, out_path) = if opt {
            let options = vec![CompileOptions::OptimizeIR];
            let out_path = out_path(test_name, "llopt");
            (options, out_path)
        } else {
            let options = vec![];
            let out_path = out_path(test_name, "ll");
            (options, out_path)
        };
        clear_output_files(&out_path);
        compile(&src_path(test_name), &out_path, &options).unwrap();

        assert!(std::fs::exists(&out_path).unwrap());
        assert!(!std::fs::exists(out_path.with_extension("o")).unwrap());
        assert!(!std::fs::exists(out_path.with_extension("out")).unwrap());
        clear_output_files(&out_path);
    }

    fn obj(test_name: &str) {
        let options = vec![CompileOptions::CreateObjFile];
        let out_path = out_path(test_name, "obj");
        clear_output_files(&out_path);
        compile(&src_path(test_name), &out_path, &options).unwrap();

        assert!(std::fs::exists(&out_path).unwrap());
        assert!(std::fs::exists(out_path.with_extension("o")).unwrap());
        assert!(!std::fs::exists(out_path.with_extension("out")).unwrap());
        clear_output_files(&out_path);
    }

    fn exec(test_name: &str, stdout_expect: &str) {
        let options = vec![CompileOptions::CreateExecutable, CompileOptions::OptimizeIR];
        let out_path = out_path(test_name, "exec");
        clear_output_files(&out_path);
        compile(&src_path(test_name), &out_path, &options).unwrap();

        assert!(std::fs::exists(&out_path).unwrap());
        assert!(std::fs::exists(out_path.with_extension("o")).unwrap());
        assert!(std::fs::exists(out_path.with_extension("out")).unwrap());

        let mut cmd = Command::new(out_path.with_extension("out"));
        let output = cmd.output().unwrap();
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert_eq!(stdout, stdout_expect);
        clear_output_files(&out_path);
    }

    fn src_path(test_name: &str) -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("ocaml")
            .join(format!("{test_name}.ml"))
    }

    fn out_path(test_name: &str, postfix: &str) -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("ocaml")
            .join(format!("test-{test_name}-{postfix}.ll"))
    }

    fn clear_output_files(out_path: &Path) {
        let _ = std::fs::remove_file(out_path);
        let _ = std::fs::remove_file(out_path.with_extension("o"));
        let _ = std::fs::remove_file(out_path.with_extension("out"));
    }
}
