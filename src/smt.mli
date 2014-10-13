(**
   Module used to define constraints
*)

module ZZ : ZZ3_sigs.S
module Opti : module type of ZZ3Optim.Make(ZZ)

type relation = ZZ.zbool ZZ.term
type expression = ZZ.znum ZZ.term


(**
   [mk_and r1 r2] returns the relation [r1 /\ r2], simplified using
   the rule of associativity and the fact that [r /\ True = True /\ r
   = r]
*)
val mk_and : relation -> relation -> relation

(**
   [mk_or r1 r2] returns the relation [r1 \/ r2], simplified using the
   rule of associativity and the fact that [r \/ False = False \/ r =
   r]
*)
val mk_or : relation -> relation -> relation

(**
   [mk_mul e1 e2] returns the expression [e1 * e2], simplified using the
   facts that [r * 0 = 0 * r = 0] and [r * 1 = 1 * r = r]
*)
val mk_mul : expression -> expression -> expression

(**
   [mk_add e1 e2] returns the expression [e1 + e2], simplified using the
   fact that [r + 0 = 0 + r = r]
*)
val mk_add : expression -> expression -> expression

(**
   [and_of_list l] returns the conjunction of the relations of the
   list [l]. This function does the same simplifications as {!mk_and}
*)
val and_of_list : relation list -> relation

(**
   [or_of_list l] returns the disjunction of the relations of the
   list [l]. This function does the same simplifications as {!mk_or}
*)
val or_of_list : relation list -> relation

(**
   Types used to store variables of a model
*)
type result =
| RState of int ref
| RVector of Q.t array
| RQ of Q.t ref
| RNone

(**
   Type representing a (partial) model
*)
type model = (string, result) Hashtbl.t

(**
   [get_result model name] returns the reference or array where the
   variable [name] can be stored (see {!result} type)
*)
val get_result : model -> string -> result

(**
   [state_eq_implies k n i r] returns the formula "there exist 0<=j<n
   such that j!=i and k=j or r", which is equivalent to "k=i => r" but
   does not use any negation (and is thus compatible with the
   generalize_model function)
*)
val state_eq_implies : expression -> int -> int -> relation -> relation


(**
  [is_int n v] returns the expression "v is a positive integer smaller
  than n"
*)
val is_int : int -> expression -> relation

(**
   [minimize cost costval e] set the value of the cost function
*)
val minimize : expression -> expression -> expression -> relation
