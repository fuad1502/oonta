use std::{collections::HashMap, env::Args};

pub struct Cmd {
    pub options: HashMap<CmdOptions, Option<String>>,
    pub free_args: Vec<String>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CmdOptions {
    Help,
    OutputPath,
    TopLevel,
    Compile,
    Exec,
}

#[derive(Debug)]
pub enum CmdError {
    OptionExpectedArgument(String),
    UnknownOption(String),
    SingleDashLongOption(String),
}

pub fn parse_arguments(args: Args) -> Result<Cmd, CmdError> {
    let mut free_args = vec![];
    let mut options = HashMap::new();
    let mut args = args.into_iter();
    while let Some(arg) = args.next() {
        match extract_option(&arg)? {
            None => free_args.push(arg),
            Some(option) if !option.accepts_arg() => _ = options.insert(option, None),
            Some(option) => {
                if let Some(arg) = args.next()
                    && !is_option(&arg)
                {
                    options.insert(option, Some(arg));
                } else {
                    return Err(CmdError::OptionExpectedArgument(arg));
                }
            }
        }
    }
    Ok(Cmd { options, free_args })
}

pub fn print_help() -> &'static str {
    // TODO: Refactor option names
    r#"OCaml to LLVM IR compiler

Usage: oonta [OPTIONS] <file>

Options:
  -h, --help                    Display this information. 
  -o <file>, --output <file>    Write output to file.
  -t, --top-level               Output main function instead of caml_main.
  -c, --compile                 Compile LLVM IR to object file using LLVM.
  -e, --exec                    Compile LLVM IR to executable. Turning this
                                option on implicitly turns on both '-t' and
                                '-c' options.
"#
}

fn extract_option(arg: &str) -> Result<Option<CmdOptions>, CmdError> {
    if !is_option(arg) {
        return Ok(None);
    }

    if is_long_option(arg) {
        match CmdOptions::try_from(&arg[2..]) {
            Ok(opt) => Ok(Some(opt)),
            Err(e) => Err(e),
        }
    } else if arg.chars().count() == 2 {
        let ch = arg.chars().nth(1).unwrap();
        match CmdOptions::try_from(ch) {
            Ok(opt) => Ok(Some(opt)),
            Err(e) => Err(e),
        }
    } else {
        Err(CmdError::SingleDashLongOption(arg.to_string()))
    }
}

fn is_option(arg: &str) -> bool {
    arg.starts_with("-")
}

fn is_long_option(arg: &str) -> bool {
    arg.starts_with("--")
}

impl TryFrom<&str> for CmdOptions {
    type Error = CmdError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "help" => Ok(Self::Help),
            "output" => Ok(Self::OutputPath),
            "top-level" => Ok(Self::TopLevel),
            "compile" => Ok(Self::Compile),
            "exec" => Ok(Self::Exec),
            _ => Err(CmdError::UnknownOption(format!("--{value}"))),
        }
    }
}

impl TryFrom<char> for CmdOptions {
    type Error = CmdError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'h' => Ok(Self::Help),
            'o' => Ok(Self::OutputPath),
            't' => Ok(Self::TopLevel),
            'c' => Ok(Self::Compile),
            'e' => Ok(Self::Exec),
            _ => Err(CmdError::UnknownOption(format!("-{value}"))),
        }
    }
}

impl CmdOptions {
    fn accepts_arg(&self) -> bool {
        match self {
            CmdOptions::OutputPath => true,
            CmdOptions::Help => false,
            CmdOptions::TopLevel => false,
            CmdOptions::Compile => false,
            CmdOptions::Exec => false,
        }
    }
}

impl std::error::Error for CmdError {}

impl std::fmt::Display for CmdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CmdError::OptionExpectedArgument(arg) => {
                write!(f, "Error: option \"{arg}\" expected an argument")
            }
            CmdError::UnknownOption(arg) => write!(f, "Error: unknown option \"{arg}\""),
            CmdError::SingleDashLongOption(arg) => {
                write!(
                    f,
                    "Error: singe dash options (\"{arg}\") only accepts a single character"
                )
            }
        }
    }
}
