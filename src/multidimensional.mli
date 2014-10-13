(**
   Multidimensional algorithm
*)

open Smt.ZZ

(**
   [multidimensional vars inv smt_inv tau verbose] returns a multidimensional
   strict ranking function [rho], or [] if it does not exist

   - [vars] is associating each variable of the automaton to a unique
   positive integer (see {!Automaton.indexed_strings})
   - [inv] contains the invariants (see {!Invariants.invariants})
   - [smt_inv] is the formula representing the invariant
   - [tau] is the formula representing the transition relation
   - If [verbose] is true, prints the values of [l], [x], [y], [x-y],
   [cost], [lambda] and [delta] at each iteration.
*)
val multidimensional :
  ?verbose:bool ->
  (bool -> Llvm.llbasicblock -> zbool term) ->
  (bool -> Llvm.llvalue -> [< znum ] term) ->
  Invariants.invariant ->
  zbool term ->
  (Q.t array * Q.t) list option
