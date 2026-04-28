# OCaml Autodiff

An efficient and extensible automatic differentiation library for OCaml, supporting both reverse and forward mode AD.

## Team Members

- Richie Xue (rx77)
- Eric Chen (ec936)
- Jayden Lim (jcl399)
- Colby Bittner (cb954)

## Project Features (MS2 WIP)

- **Reverse-Mode AD:** Optimized for expressions with many parameters.
- **Forward-Mode AD:** Implemented via dual numbers for specific partial derivatives.
- **Expression Parsing:** Robust recursive-descent parser for arithmetic (`+`, `-`, `*`, `/`, `^`) and functions (`exp`, `ln`, `sin`, `cos`).
- **Gradient Checker:** Numerical verification suite to ensure AD accuracy.
- **Visualization:** Export expression computation graphs to Graphviz DOT format.
- **ML Training Engine:** Stochastic Gradient Descent (SGD) and Adam optimizer prototypes for linear models.

## Setup Instructions

Clone the repository:
```bash
git clone https://github.com/Keobkeig/ocaml-autodiff.git
cd ocaml-autodiff
```

Build the project:
```bash
dune build
```

## Usage

Use the CLI to differentiate expressions, train models, or export visualizations.

### 1. Differentiate (Reverse Mode)
```bash
dune exec bin/main.exe -- diff "x*x + 3*x + 1" "x=2"
```

### 2. Differentiate (Forward Mode)
```bash
dune exec bin/main.exe -- forward-diff "x*y + exp(x)" "x=1,y=2"
```

### 3. Run Training Demo
```bash
dune exec bin/main.exe -- train 300 0.05
```

### 4. Export Visualization
```bash
dune exec bin/main.exe -- export-dot "x*x + sin(x)" expr.dot
# Render with: dot -Tpng expr.dot -o expr.png
```

### 5. Check Gradients
```bash
dune exec bin/main.exe -- check-grad "x*x" "x=2" x
```

## Testing & Coverage

Run the test suite:
```bash
dune test
```

Generate coverage report (requires `bisect_ppx`):
```bash
dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html
```

## MS2 Submission Files

- `INSTALL.md`: Detailed installation and environment setup.
- `AUTHORS.md`: Project contributors and citations.
- `gallery.yaml`: Project gallery metadata and demo link.
- `RepoURL.txt`: Official repository URL.
- `MS2Report.txt`: Progress report (to be converted to PDF).

