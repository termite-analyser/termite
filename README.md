# Termite

Termite is a tool to check termination of programs in the LLVM IR.
It uses pagai as invariant generator and Z3 as SMT-solver.

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

## How-to

Using opam:

0. Install llvm 3.6 from your distribution packages and [pagai](http://pagai.forge.imag.fr/).

1. Install the branch of Z3 with optimization and the ml binding
   Install [this fork](https://z3.codeplex.com/SourceControl/network/forks/gradanne/mlopti?branch=optiz3-ml) with the branch `optiz3-ml`. You can install by doing :
   ```
   python2 scripts/mk_make.py --ml
   cd build
   make -j4
   make ocamlfind_install
   sudo make install
   ```

2. Install Z3 overlay
   ```
   opam repository add drupam "https://github.com/Drup/drupam.git"
   opam install z3overlay
   ```

3. install the various libraries:
   ```
   opam install llvm ocamlgraph batteries zarith llvm2smt
   ```

4. Pin and install termite:
   ```
   opam pin add termite https://github.com/termite-analyser/termite.git`.
   ```
