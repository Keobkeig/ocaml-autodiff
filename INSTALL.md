# INSTALL

This file explains how a grader can install dependencies, build, test, and run
the OCaml Autodiff project.

## 1) Prerequisites

- OPAM installed and initialized
- OCaml compiler available in the active switch
- Dune build system

If needed:

```bash
opam update
opam install ocaml dune ounit2
```

## 2) Build

From the project root:

```bash
dune build
```

## 3) Run tests

```bash
dune test
```

## 4) Run the executable

Show command help:

```bash
dune exec bin/main.exe -- help
```

Differentiate an expression:

```bash
dune exec bin/main.exe -- diff "x*x + 3*x + 1" "x=2"
```

Run the training demo:

```bash
dune exec bin/main.exe -- train 300 0.05
```

## 5) Notes

- The project currently uses only OCaml + OUnit2 from OPAM in addition to Dune.
- If command resolution for public executable names varies by environment,
  prefer `dune exec bin/main.exe -- ...`.
