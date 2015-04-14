# Termite

Termite is a tool to check termination of programs in the LLVM IR.
It uses pagai as invariant generator and Z3 as SMT-solver.

## Install and dependencies

### With opam
Using opam, you can install (most of) the dependencies easily. See the install instructions [here](https://github.com/termite-analyser/opam-termite).

### Dependencies

External dependencies:
- Z3
- Pagai

Ocaml dependencies:
- batteries-included
- Zarith
- ocamlgraph
- [llvmgraph](https://github.com/Drup/llvmgraph)
- [z3overlay](https://github.com/termite-analyser/z3overlay)
- [llvm2smt](https://github.com/termite-analyser/llvm2smt)
