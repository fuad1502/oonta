![Oonta: OCaml to LLVM IR Compiler](assets/banner.png)

[![CI](https://github.com/fuad1502/oonta/actions/workflows/CI.yml/badge.svg)](https://github.com/fuad1502/oonta/actions/workflows/CI.yml)

*Oonta* is a compiler front-end for the [OCaml programming
language](https://ocaml.org): it generates [LLVM intermediate representation
(IR)](https://llvm.org/docs/LangRef.html) from OCaml source code.

*Oonta* uses the [JJIK](https://github.com/fuad1502/jjik) parser generator and
[JLEK](https://github.com/fuad1502/jlek) lexer generator to perform the parsing
and lexing stages.

> [!IMPORTANT]
> This project is still a work in progress, many OCaml features are not yet
> supported. For example, polymorphic functions and types, and modules are not
> yet supported. Additionally, the garbage collector runtime is not yet
> available. See the issues tab for the list of work items.

> [!NOTE]
> This project is part of the ["Compiler
> Toys"](https://github.com/fuad1502/compiler_toys) project, originally meant
> as a learning exercise on Compilers.

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
## Benchmark

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

let rec create_lst_aux n acc =
  match n with
  | 0 -> acc
  | _ -> create_lst_aux (n - 1) (Cat (read_int (), acc))

let create_lst n = create_lst_aux n Empty
let n = read_int ()
let lst = create_lst n
let sorted_lst = merge_sort lst
let () = map print_int sorted_lst
```
And insertion sort (`benchmark/insertion_sort.ml`):

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

let rec insert elem lst =
  match lst with
  | Empty -> Cat (elem, Empty)
  | Cat (head, tail) ->
      if elem <= head then
        Cat (elem, lst)
      else
        Cat (head, insert elem tail)

let rec insertion_sort lst =
  match lst with
  | Empty -> Empty
  | Cat (head, tail) -> insert head (insertion_sort tail)

let create_lst n = create_lst_aux n Empty
let n = read_int ()
let lst = create_lst n
let sorted_lst = insertion_sort lst
let () = map print_int sorted_lst
```
Execute the following command inside the repository root folder to run the
benchmarks.

```sh
python3 benchmark/benchmark.py 1000000 0 # run the merge_sort.ml benchmark on a list with 1 million elements
python3 benchmark/benchmark.py 10000 0 # run the insertion_sort.ml benchmark on a list with 10000 elements
```

> [!IMPORTANT] 
> Currently, for running the benchmark on large inputs, we have to increase the
> stack size with `ulimit -s unlimited`

On my machine (AMD Ryzen™ 7 7700X × 16), the result of running the above
benchmarks are as follows:

```text
Benchmarking: merge_sort.ml
Elapsed time (./benchmark/ocamlopt.out): 0.8648 seconds
Elapsed time (./benchmark/oonta.out): 1.5996 seconds
> 1.85 times slower

Benchmarking: insertion_sort.ml
Elapsed time (./benchmark/ocamlopt.out): 0.1317 seconds
Elapsed time (./benchmark/oonta.out): 0.8740 seconds
> 6.64 times slower
```
One probable reason for the slow down is because (currently) it constantly
request for memory allocations through `malloc` calls for each tuple and
variant construction expression. Additionally, since there is no garbage
collector, the memory usage will be very high and it would not use the cache
very effectively.

By running `time` on `oonta.out` and `ocamlopt.out`, we could see that most of
the excess time is used by system calls.

```text
# /usr/bin/time -v benchmark/oonta.out < benchmark/input.txt > benchmark/output.txt   
        Command being timed: "benchmark/oonta.out"
        User time (seconds): 0.92
        System time (seconds): 0.65
        Percent of CPU this job got: 100%
        Elapsed (wall clock) time (h:mm:ss or m:ss): 0:01.58
        ...
        Maximum resident set size (kbytes): 2603072
        ...
        Minor (reclaiming a frame) page faults: 650487
        ...
# /usr/bin/time -v benchmark/ocamlopt.out < benchmark/input.txt > benchmark/output.txt
        Command being timed: "benchmark/ocamlopt.out"
        User time (seconds): 0.77
        System time (seconds): 0.08
        Percent of CPU this job got: 99%
        Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.85
        ...
        Maximum resident set size (kbytes): 257520
        ...
        Minor (reclaiming a frame) page faults: 67946
        ...
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
On Ubuntu, install the `llvm` package to make those commands available.

```sh
sudo apt install llvm
```
> [!NOTE]
> I will be working on my own LLVM backend as part of my compiler learning
> journey! ✨

> [!WARNING]
> The generated IR uses [opaque
> pointers](https://llvm.org/docs/OpaquePointers.html). If your LLVM version is
> older than version 15, the `--compile` / `--exec` options might not work.

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
