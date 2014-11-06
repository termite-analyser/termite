(**
   Monodimensional algorithm
*)

open Smt.ZZ

(**
   [monodimensional vars inv rel rho verbose] returns the next linear
   component [l] of a weak ranking function, such that [pi(l)] is
   maximal.

   - [vars] is associating each variable of the automaton to a unique
   positive integer (see {!Automaton.indexed_strings})
   - [inv] contains the invariants (see {!Invariants.invariants})
   - [rel] is the formula that the variables have to respect, i.e. I /\ tau.
   - If [verbose] is true, prints the values of [l], [x], [y], [x-y],
   [cost], [lambda] and [delta] at each iteration.
*)
val monodimensional :
  ?u:(Q.t, Smt.ZZ.znum) symbol array ->
  ?verbose:bool ->
  (bool -> Llvm.llbasicblock -> zbool term) ->
  (bool -> Llvm.llvalue -> [< znum ] term) ->
  Invariants.invariant ->
  zbool term ->
  (Q.t array * Q.t * bool) Debug.result
