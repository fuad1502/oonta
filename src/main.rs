use std::{env, path::PathBuf, process::ExitCode};

use oonta::{
    cmd::CmdOptions,
    driver::CompileOptions,
    terminal_colors::{END, RED},
};

fn main() -> ExitCode {
    let cmd = match oonta::cmd::parse_arguments(env::args()) {
        Ok(cmd) => cmd,
        Err(e) => {
            eprintln!("{e}");
            return ExitCode::FAILURE;
        }
    };

    if cmd.options.contains_key(&CmdOptions::Help) {
        println!("{}", oonta::cmd::print_help());
        return ExitCode::SUCCESS;
    }

    if cmd.free_args.len() < 2 {
        eprintln!("{RED}Error{END}: no input file");
        return ExitCode::FAILURE;
    }
    if cmd.free_args.len() > 2 {
        eprintln!("{RED}Error{END}: oonta only accepts a single input file");
        return ExitCode::FAILURE;
    }
    let src_file = PathBuf::from(&cmd.free_args[1]);

    let out_file = if let Some(Some(path)) = cmd.options.get(&CmdOptions::OutputPath) {
        PathBuf::from(path)
    } else {
        src_file.with_extension("ll")
    };

    let mut compile_options = vec![];
    if cmd.options.contains_key(&CmdOptions::TopLevel) {
        compile_options.push(CompileOptions::TopLevel);
    }
    if cmd.options.contains_key(&CmdOptions::Compile) {
        compile_options.push(CompileOptions::CreateObjFile);
    }
    if cmd.options.contains_key(&CmdOptions::Exec) {
        compile_options.push(CompileOptions::CreateExecutable);
    }
    if cmd.options.contains_key(&CmdOptions::Verbose) {
        compile_options.push(CompileOptions::DebugPhases);
    }

    match oonta::driver::compile(&src_file, &out_file, &compile_options) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}
