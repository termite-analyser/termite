(**
   First (non-terminating) algorithm
*)

open Smt.ZZ

(**
   Exception raised when the algorithm makes too much iterations
*)
exception Timeout

(**
   [algo1 block_dict var_dict invariant tau timeout verbose] returns the linear
   component [l] of a weak ranking function, such that [pi(l)] is
   maximal.

   - [block_dict] and [var_dict] are dictionaries respectively from basic blocks and llvm variables to smt variables.
   positive integer (see {!Automaton.indexed_strings})
   - [invariant] is the the invariant (see {!Invariants.invariants})
   - [tau] is the formula representing the transition relation
   - If [timeout] is a positive integer, the algorithm will raise the
   [Timeout] exception at the [timeout]-th iteration. Default is [10].
   - If [verbose] is true, prints the values of [l], [x], [y], [x-y],
   [cost], [lambda] and [delta] at each iteration. Default is [false]

   {b WARNING} : This algorithm does not always terminate. See
   {!Monodimensional.monodimensional} for a terminating algorithm.
*)
val algo1 :
  ?verbose:bool ->
  ?timeout:int ->
  (bool -> Llvm.llbasicblock -> zbool term) ->
  (bool -> Llvm.llvalue -> [< znum ] term) ->
  Invariants.invariant ->
  zbool term
  -> Q.t array * Q.t
