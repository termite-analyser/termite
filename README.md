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
    - [ocaml-z3](https://github.com/drup/ocaml-z3)
    - The internal library: llvm2smt

## How-to

Using opam:
    0. Install llvm from your distribution packages and [pagai](http://pagai.forge.imag.fr/).

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
       opam install llvm ocamlgraph batteries zarith
       ```

    4. Pin and install the library in this repository. From the root of the stator repository:
       ```
       opam pin add llvm2smt stator/llvm2smt/
       opam pin add termite stator/smt_terminate/`.
       ```
       If you use a previous opam version, you may need to remove the `add` in the two previous commands.
