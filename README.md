# OCaml Autodiff

## Team Members

- [Richie Xue] ([rx77])
- [Eric Chen] ([ec936])
- [Jayden Lim ] ([jcl399])
- [Colby Bittner] ([cb954])

## Project Description

Autodiff engine prototype in OCaml for CS 3110.

Sprint A delivers:
- Scalar reverse-mode automatic differentiation for expression trees.
- Expression parser for arithmetic and selected math functions.
- CLI command to evaluate expressions and gradients.
- Gradient-descent training demo for a linear regression model.

## Setup Instructions

```bash
cd 3110-final-project
dune build
```

## Build and Run

```bash
dune exec bin/main.exe -- help
```

You can also run the installed public executable name:

```bash
dune exec ocaml-autodiff -- help
```

### Differentiate an expression

```bash
dune exec bin/main.exe -- diff "x*x + 3*x + 1" "x=2"
```

### Run training demo

```bash
dune exec bin/main.exe -- train 300 0.05
```

## Testing

```bash
dune test
```

## MS2 Submission Files

The repository includes draft/required MS2 artifacts:

- `INSTALL.md`
- `AUTHORS.md`
- `gallery.yaml`
- `MS2Report.md` (export to `MS2Report.pdf` for submission)
- `RepoURL.txt`
