(**
   Module used to define constraints
*)


module ZZ = ZZ3.Make(struct let ctx = Z3.mk_context [] end)
open ZZ

module Opti = ZZ3Optim.Make (ZZ)

type relation = zbool term
type expression = znum term

(** [mk_and r1 r2] returns the relation [r1 /\ r2]. *)
let mk_and r1 r2 = T.(r1 && r2)

(** [mk_or r1 r2] returns the relation [r1 \/ r2]. *)
let mk_or r1 r2 = T.(r1 || r2)

(** [mk_mul e1 e2] returns the expression [e1 * e2]. *)
let mk_mul e1 e2 = T.(e1 * e2)

(** [mk_add e1 e2] returns the expression [e1 + e2]. *)
let mk_add e1 e2 = T.(e1 + e2)

(** [and_of_list l] returns the conjunction of the list [l]. *)
let and_of_list x = T.and_ x

(** [or_of_list l] returns the disjunction of the list [l]. *)
let or_of_list x = T.or_ x

let state ~k ~kp b =
  if b then kp else k

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
let get_result model name =
  try
    Hashtbl.find model name
  with
    | Not_found -> RNone


(**
   [state_eq_implies k n i r] returns the formula "there exist 0<=j<n
   such that j!=i and k=j or r", which is equivalent to "k=i => r" but
   does not use any negation (and is thus compatible with the
   generalize_model function)
*)
let rec state_eq_implies k n i r =
  if n < 0 then
    r
  else
    if n != i then
      let n' = n - 1 in
      T.(k = T.rat (Q.of_int n) || state_eq_implies k n' i r)
    else
      state_eq_implies k (n-1) i r

(**
   [is_int n v] returns the expression "v is a positive integer smaller
   than n"
*)
let rec is_int n v =
  if n < 0 then
    T.false_
  else
    mk_or
      T.(v = T.rat (Q.of_int n))
      (is_int (n-1) v)

(**
   [minimize cost costval e] set the value of the cost function
*)
let minimize cost costval e =
  T.( cost = costval && costval = e)
