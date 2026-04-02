[<img alt="crates.io" src="https://img.shields.io/crates/v/oonta.svg">](https://crates.io/crates/oonta)
[![CI](https://github.com/fuad1502/oonta/actions/workflows/CI.yml/badge.svg)](https://github.com/fuad1502/oonta/actions/workflows/CI.yml)

![Oonta: OCaml to LLVM IR Compiler](assets/banner.png)

## Table of Contents

* [Introduction](#introduction)
* [Quick Start (Ubuntu)](#quick-start-ubuntu)
* [Benchmark against `ocamlopt`](#benchmark-against-ocamlopt)
* [User Guide](#user-guide)
* [Dependencies](#dependencies)
* [Feature Highlights](#feature-highlights)
    * [Supported Ocaml language features](#supported-ocaml-language-features)
    * [Compile-time monomorphization](#compile-time-monomorphization)
    * [Debug compile phases](#debug-compile-phases)
    * [Error reporting](#error-reporting)
* [Building from source](#building-from-source)
* [Why is it called Oonta?](#why-is-it-called-oonta)

## Introduction

*Oonta* is a compiler front-end for a subset of the [OCaml programming
language](https://ocaml.org): it generates [LLVM intermediate representation
(IR)](https://llvm.org/docs/LangRef.html) from OCaml source code.

*Oonta* uses the [JJIK](https://github.com/fuad1502/jjik) parser generator and
[JLEK](https://github.com/fuad1502/jlek) lexer generator to perform the parsing
and lexing stages. For building the IR, *Oonta* does not depend on the LLVM
API.

*This project is still a work in progress*, many OCaml features are not yet
supported. For example, modules, classes and objects are not yet supported.
Additionally, the runtime does not yet provide a garbage collection service.
Checkout [this section](#supported-ocaml-language-features) for a complete list
of currently supported language features.

> [!NOTE]
> One could argue that with the many OCaml key features currently missing, it
> is not appropriate to call it "a compiler for a subset of the OCaml
> language", since it is more akin to simpler dialects of the ML programming
> language family, such as SML or Caml. However, the implementation and syntax
> reference has always been OCaml. Moreover, the final goal is to support as
> much OCaml features possible.

> [!NOTE]
> This project is part of the ["Compiler
> Toys"](https://github.com/fuad1502/compiler_toys) project, originally meant
> as a learning exercise on Compilers.

## Quick Start (Ubuntu)

[Install Rust](https://rust-lang.org/tools/install/) and LLVM (`sudo apt
install llvm`), then:

```sh
cargo install oonta # installs the oonta command
wget https://github.com/fuad1502/oonta/releases/download/v0.2.0/liboonta_runtime-0.2.0-Linux.deb
sudo dpkg -i liboonta_runtime-0.2.0-Linux.deb # installs oonta runtime library
cat << EOF > main.ml
let rec factorial x = if x <= 1 then 1 else x * factorial (x - 1)
let () = print_int (factorial 5)
EOF
oonta --opt --exec main.ml
./main.out # prints `120`
```
## Benchmark against `ocamlopt`

Two benchmarks are provided:

Merge sort (`benchmark/merge_sort.ml`):

```ocaml
type list =
  | Empty
  | Cat of (int * list)

let rec map f lst =
  match lst with
  | Empty -> ()
  | Cat (x, l) ->
      let () = f x in
      map f l

let rec create_lst_aux n acc =
  match n with
  | 0 -> acc
  | _ -> create_lst_aux (n - 1) (Cat (read_int (), acc))

let create_lst n = create_lst_aux n Empty

let rec split_aux lst acc_left acc_right =
  match lst with
  | Empty -> (acc_left, acc_right)
  | Cat (head, rest) -> split_aux rest acc_right (Cat (head, acc_left))

let split lst = split_aux lst Empty Empty

let rec merge left right =
  match (left, right) with
  | Empty, right -> right
  | lest, Empty -> lest
  | Cat (left_head, left_rest), Cat (right_head, right_rest) ->
      if left_head <= right_head then
        Cat (left_head, merge left_rest right)
      else
        Cat (right_head, merge left right_rest)

let rec merge_sort lst =
  match lst with
  | Empty -> lst
  | Cat (_, Empty) -> lst
  | _ ->
      let splitted_lst = split lst in
      let left =
        match splitted_lst with
        | lst, _ -> lst
      in
      let right =
        match splitted_lst with
        | _, lst -> lst
      in
      merge (merge_sort left) (merge_sort right)

let n = read_int ()
let lst = create_lst n
let sorted_lst = merge_sort lst
let () = map print_int sorted_lst
```
Insertion sort (`benchmark/insertion_sort.ml`):

```ocaml
type list =
  | Empty
  | Cat of (int * list)

let rec map f lst =
  match lst with
  | Empty -> ()
  | Cat (x, l) ->
      let () = f x in
      map f l

let rec create_lst_aux n acc =
  match n with
  | 0 -> acc
  | _ -> create_lst_aux (n - 1) (Cat (read_int (), acc))

let create_lst n = create_lst_aux n Empty

let rec insert elem lst =
  match lst with
  | Empty -> Cat (elem, lst)
  | Cat (head, tail) ->
      if elem <= head then
        Cat (elem, lst)
      else
        Cat (head, insert elem tail)

let rec insertion_sort lst =
  match lst with
  | Empty -> lst
  | Cat (head, tail) -> insert head (insertion_sort tail)

let n = read_int ()
let lst = create_lst n
let sorted_lst = insertion_sort lst
let () = map print_int sorted_lst
```
And polymorphic list comparison (`benchmark/polymorphic_compare.ml`):

```ocaml
type 'a list =
  | Empty
  | Cat of ('a * 'a list)

let rec create_lst_aux n acc =
  match n with
  | 0 -> acc
  | _ -> create_lst_aux (n - 1) (Cat (read_int (), acc))

let create_lst n = create_lst_aux n Empty

let compare lst_a lst_b =
  if lst_a > lst_b then
    print_int 1
  else if lst_a = lst_b then
    print_int 0
  else
    print_int 2

let n = read_int ()
let lst = create_lst n
let () = compare lst lst
```
Execute the following command inside the repository root folder to run the
benchmarks.

```sh
python3 benchmark/benchmark.py 1000000 0 # run the merge_sort.ml benchmark on a list with 1 million elements
python3 benchmark/benchmark.py 10000 1 # run the insertion_sort.ml benchmark on a list with 10000 elements
python3 benchmark/benchmark.py 1000000 2 # run the polymorphic_compare.ml benchmark on a list with 10000 elements
```

> [!WARNING] 
> Currently, for running the merge sort benchmark on large inputs, increase the
> stack size limit with `ulimit -s unlimited`

On my Ubuntu machine (AMD Ryzen™ 7 7700X × 16), with `ocamlopt` version 5.4.0
and LLVM version 20.1.8, the result of running the above benchmarks are as
follows:

```text
Benchmarking: merge_sort.ml
Elapsed time (./benchmark/ocamlopt.out): 0.8747 seconds
Elapsed time (./benchmark/oonta.out): 0.7445 seconds
> 14.88% faster

Benchmarking: insertion_sort.ml
Elapsed time (./benchmark/ocamlopt.out): 0.1325 seconds
Elapsed time (./benchmark/oonta.out): 0.3481 seconds
> 262.68% slower

Benchmarking: polymorphic_compare.ml
Elapsed time (./benchmark/ocamlopt.out): 0.1026 seconds
Elapsed time (./benchmark/oonta.out): 0.0635 seconds
> 38.14% faster
```
## User Guide

```sh
oonta --help
```
## Dependencies

With no command line options given, the `oonta` command will simply generates
LLVM IR without requiring any runtime dependencies. However, it provides
`--opt, -O`, `--compile / -c` and `--exec / -e` options to optimize the
generated IR, compile the generated IR to an object code and executable,
respectively, which requires certain dependencies to function. Internally,
`oonta` will invoke the following commands when given those options:

```sh
# with --opt
opt -S -O3 -o <.ll file> <.ll file>
# with --compile
llc -O3 -relocation-model=pic --filetype=obj -o <output> <.ll file>
# with --exec
clang -o <output> <.o file> -loonta_runtime
```
On Ubuntu, install the `llvm` package to make `opt`, `llc`, and `clang`
available.

```sh
sudo apt install llvm
```
> [!WARNING]
> The generated IR uses [opaque
> pointers](https://llvm.org/docs/OpaquePointers.html). If the LLVM version is
> older than version 15, the `--opt / --compile` / `--exec` options might not
> work. If you're on Debian/Ubuntu, see
> [https://apt.llvm.org/](https://apt.llvm.org/) for instructions on installing
> other versions.

Additionally, with `--exec`, you need the *Oonta runtime library*:
`liboonta_runtime.a`. If you're running an x64 linux machine, you can obtain
the static library from the [release
page](https://github.com/fuad1502/oonta/releases). If not, you would need to
build the runtime from source by following the guide
[below](#building-from-source).

> [!NOTE]
> Currently the runtime only provides memory allocation requests using *bump
> allocation*. Garbage collection service is not yet available.

## Feature Highlights

### Supported OCaml language features

> [!NOTE]
> This list is by no means complete. Unsupported features were intentionally
> not fully expanded for brevity.

| Feature                        | Supported?                             |
|--------------------------------|----------------------------------------|
| Basic data types               | Integers, float and booleans           |
| Type inference                 | Yes                                    |
| Global definitions             | Yes                                    |
| Local definitions              | Yes                                    |
| First class functions          | Yes                                    |
| Closures                       | Yes                                    |
| Partial applications           | Yes                                    |
| Recursive definitions          | Yes                                    |
| Parametric variants            | Yes                                    |
| Polymorphic functions          | Yes                                    |
| Anonymous functions            | Yes                                    |
| Conditionals                   | Yes                                    |
| Pattern matching               | Yes. However, no exhaustiveness check. |
| Mutually recursive definitions | No                                     |
| Records                        | No                                     |
| Imperative features            | No                                     |
| Exceptions                     | No                                     |
| Lazy expressions               | No                                     |
| Module system                  | No                                     |
| Classes and objects            | No                                     |
| Labeled arguments              | No                                     |
| Polymorphic variants           | No                                     |

### Compile-time monomorphization

Oonta handles (parameteric) polymorphic function through compile-time
monomorphization. It therefore generates more optimized code, at the cost of
code bloat. This also allows removing the need for pointer tag.

For example, for the following OCaml code:

```ocaml
let apply_on_greater f x y =
  if x > y then
    f x
  else
    f y

let () = apply_on_greater print_int 3 2
```
It will monomorphize `apply_on_greater` with type `(('a -> 'b) -> 'a -> 'a ->
'b)` into `((int -> unit) -> int -> int -> unit)` and generate the following
LLVM IR:

```llvm
define ccc void @oonta.apply_on_greater.int.unit.fun(ptr %f, i64 %x, i64 %y, ptr %env)  {
entry:
    %r = icmp sgt i64 %x, %y
    br i1 %r, label %then, label %else
then:
    %r0 = load ptr, ptr %f
    call ccc void %r0(i64 %x, ptr %f)
    br label %follow
else:
    %r1 = load ptr, ptr %f
    call ccc void %r1(i64 %y, ptr %f)
    br label %follow
follow:
    ret void
}
```
### Debug compile phases

Use the `--verbose / -v` option to debug each compile phase.

```sh
cat << EOF > main.ml
let apply_on_greater f x y =
  if x > y then
    f x
  else
    f y

let () = apply_on_greater print_int 3 2
EOF
oonta --exec --opt -v main.ml
```

```text
=> Lexing & Parsing Start
=> Lexing & Parsing End (0 ms)
=> Build AST Start
apply_on_greater = 
FunExpr
├─▸ parameters: [f, x, y]
├─▸ captures: []
├─▸ recursive: no
└─▸ body:
    CondExpr
    ├─▸ condition:
    │   BinOpExpr
    │   ├─▸ operator: >
    │   ├─▸ lhs:
    │   │   VarExpr ("x")
    │   └─▸ rhs:
    │       VarExpr ("y")
    ├─▸ then expr:
    │   ApplicationExpr
    │   ├─▸ function:
    │   │   VarExpr ("f")
    │   └─▸ binds:
    │       └─▸ (0)
    │           VarExpr ("x")
    └─▸ else expr:
        ApplicationExpr
        ├─▸ function:
        │   VarExpr ("f")
        └─▸ binds:
            └─▸ (0)
                VarExpr ("y")

() = 
ApplicationExpr
├─▸ function:
│   VarExpr ("apply_on_greater")
└─▸ binds:
    ├─▸ (0)
    │   VarExpr ("print_int")
    ├─▸ (1)
    │   LiteralExpr (3)
    └─▸ (2)
        LiteralExpr (2)

=> Build AST End (0 ms)
=> Infer types Start
Top level bindings:
apply_on_greater: (('a -> 'b) -> 'a -> 'a -> 'b)
=> Infer types End (0 ms)
=> Currying expressions Start
=> Currying expressions End (0 ms)
=> Monomorphization Start
Monomorphing 'apply_on_greater' (('a -> 'b) -> 'a -> 'a -> 'b) into ((int -> unit) -> int -> int -> unit):
'a -> int
'b -> unit
=> Monomorphization End (0 ms)
=> Build LLVM module Start
=> Build LLVM module End (0 ms)
=> Write LLVM module Start
=> Write LLVM module End (0 ms)
=> Optimize LLVM IR Start
=> Optimize LLVM IR End (12 ms)
=> LLVM backend Start
=> LLVM backend End (37 ms)
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

1. Install C++ build dependencies

```
sudo apt install build-essential cmake
```
2. Install `cargo` tool

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```
3. Clone repository

```sh
git clone https://github.com/fuad1502/oonta.git
```
4. Build `liboonta_runtime.a`

```
cmake -S runtime -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
```
5. Install `liboonta_runtime.a`

```
sudo cmake --install build
```
6. Build `oonta`

```sh
cargo build
cargo test
```
## Why is it called Oonta?

*Oonta*, is based on the Indonesian word *unta*, which translates to "camel".
