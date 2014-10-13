# Hacking termite


~~ Beware ~~


## Organization

Utilities:
    - `Debug`: Printers.
    - `Config`: Execution flags.

Base modules:
    - `Smt`: All the operation to construct and manipulate SMT formulas, using Z3 and Z3overlay.
    - `Vector`: Various utilities for operation on vectors. Also contains function to manipulate vector of smt variables.
    - `Subspace`: Simple interface to compute a subspace iteratively.
    - `Invariants`: Retrieve and export invariants inferred by pagai (given an llvm bitcode).

The actual algorithms:
    - `Lp`: Encode the LP problem in smt formulas.
    - `Algo1`, `Monodimensional`, `Multidimensional`, `MonodimMultiPc`: If you don't know, go read the paper.


The great organizer: `Main`. It's a slight mess.


## Tests and Buggyness

No automated testing (unfortunately) but a good amount of benchmarks in the `WTC` directory.

Things that might still be bugged, by decreasing order of potential bugginess:
    - The procedure for unbounded rays.
    - The multi-dimension algorithm.
    - The multi-control-point algorithm.
    - Invariants extraction on exotic CFGs, see llvm2smt too.

## TODO

    - Clean up `Smt`, `Config` and `Main`
    - Improve `Debug` and introduce a proper logging system.
    - Automate testing
    - Call clang and pagai automatically, when given a c file.
    - Factor out the common part in all algorithms.
    - Use a real LP solver for lp problems.
