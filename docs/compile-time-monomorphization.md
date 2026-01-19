# Compile-time Monomorphization

## Introduction

Compile-time monomorphization is a technique used to support polymorphic
functions, in particular, parametric polymorphic functions. Parametric
polymorphic functions are functions that are defined once, but can be called
with arguments of different types. In C++, it's called function templates, in
C#, it's called generic methods, while in OCaml, it's called polymorphic
functions:

```cpp
```

```cs
```

```ocaml
```
However, different CPU instruction(s) are needed to compare values of different
types. So, how does a single piece of code operate on different data types?
There are two methods: Dynamic Dispatch & Monomorphization.

## Implementation

## Results
