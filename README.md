# Oonta 🐪

[![CI](https://github.com/fuad1502/oonta/actions/workflows/CI.yml/badge.svg)](https://github.com/fuad1502/oonta/actions/workflows/CI.yml)

*Oonta* is a compiler front-end for the *OCaml* programming language for
generating *LLVM* intermediate representation (IR) from *OCaml* source code.

> [!IMPORTANT]
> This project is still a work in progress, many OCaml features are not yet
> supported. For example, custom types, pattern matching, and modules are not
> yet supported. Additionally, the garbage collector runtime is not yet
> available. See the issues tab for list of work items and progress.

> [!NOTE]
> This project is part of my ["Compiler Toys"
> project](https://github.com/fuad1502/compiler_toys), originally meant as a
> learning exercise on Compilers.

## Quick Start

```sh
cargo install oonta
cat << EOF > main.ml
let square x = x * x
let a = square 3
let () = print_int a
let rec factorial x = if x <= 1 then 1 else x * factorial (x - 1)
let b = factorial 5
let () = print_int b
EOF
oonta --exec main.ml
./main.out
# 9
# 120
```
## Dependencies

The `oonta` binary does not have any runtime dependencies other than the
standard library. However, for convenience, the `oonta` command provides the
`--compile / -c` and `--exec / -e` options to compile the generated IR to an
object code and executable, respectively. Internally, `oonta` will invoke the
following commands:

```sh
# with --compile
llc -relocation-model=pic --filetype=obj -o <output> <.ll file>
# with --exec
clang -o <output> <.o file>
```
On Ubuntu, install the `llvm` and `clang` package to make those commands
available.

```sh
sudo apt install llvm clang
```
> [!NOTE]
> I will be working on my own LLVM backend as part of my compiler learning
> journey! ✨

## User Guide

```sh
oonta --help
```
## Feature Highlights

### Debug compile phases

Use the `--verbose / -v` option to debug each compile phase.

```sh
cat << EOF > main.ml
let rec factorial x = if x <= 1 then 1 else x * factorial (x - 1)
let () = print_int (factorial 5)
EOF
oonta --exec -v main.ml
```

```text
=> Lexing & Parsing Start
=> Lexing & Parsing End (1 ms)
=> Build AST Start
factorial = 
FunExpr
├─▸ parameters: [x]
├─▸ captures: []
├─▸ recursive: yes
└─▸ body:
    CondExpr
    ├─▸ condition:
    │   BinOpExpr
    │   ├─▸ operator: <=
    │   ├─▸ lhs:
    │   │   VarExpr ("x")
    │   └─▸ rhs:
    │       LiteralExpr (1)
    ├─▸ then expr:
    │   LiteralExpr (1)
    └─▸ else expr:
        BinOpExpr
        ├─▸ operator: *
        ├─▸ lhs:
        │   VarExpr ("x")
        └─▸ rhs:
            ApplicationExpr
            ├─▸ function:
            │   VarExpr ("factorial")
            └─▸ binds:
                └─▸ (0)
                    BinOpExpr
                    ├─▸ operator: -
                    ├─▸ lhs:
                    │   VarExpr ("x")
                    └─▸ rhs:
                        LiteralExpr (1)

() = 
ApplicationExpr
├─▸ function:
│   VarExpr ("print_int")
└─▸ binds:
    └─▸ (0)
        ApplicationExpr
        ├─▸ function:
        │   VarExpr ("factorial")
        └─▸ binds:
            └─▸ (0)
                LiteralExpr (5)

=> Build AST End (0 ms)
=> Resolve types Start
Top level bindings:
factorial: (int -> int)
=> Resolve types End (0 ms)
=> Transform application expressions Start
=> Transform application expressions End (0 ms)
=> Build LLVM module Start
=> Build LLVM module End (0 ms)
=> Write LLVM module Start
=> Write LLVM module End (1 ms)
=> LLVM backend Start
=> LLVM backend End (117 ms)
```

### Error reporting

```text
Line   1|let x = foo 3
                 ^--
Error: cannot infer expression type: Unbound value foo
```
```text
Line   1|let rec f x = f
                       ^
Error: cannot infer expression type: Cannot unify 'b with ('a -> 'b)
```
```text
Line   1|let () = 1 + 2
                  ^----
Error: cannot bind expression of type int to ()
```

## Building from source

1. Install `cargo` tool:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

2. Clone repository.

```sh
git clone https://github.com/fuad1502/oonta.git
```
3. Build `oonta` crate.

```sh
cd compiler_toys/oonta
cargo build
cargo test
```
> [!NOTE]
> `oonta` only depends on `jjik`, `jlek`, and Rust's standard library for
> building.

## Why is it called Oonta?

*Oonta*, is based on the Indonesian word *unta*, which translates to "camel".
