(** Simple vector operations on [Q.t array] and ['a ZZ.t array] *)

open Smt
open Smt.ZZ

(** [isNull v] returns [true] if [v] is the null vector *)
let isNull t =
  not @@ BatArray.exists (fun q -> not @@ Q.equal Q.zero q) t

(**
   [hasNoNullExcept v r] returns [true] if v has no null coordinate,
   except the ones in [r], where [r] is a list of integers
*)
let rec hasNoNullExceptAux t r n i = match r with
  | j::q when j = i -> hasNoNullExceptAux t q n (i-1)
  | _ ->
      if i <= 0 then
        true
      else if Q.equal t.(n-i) Q.zero then
        false
      else
        hasNoNullExceptAux t r n (i-1)

let hasNoNullExcept t r =
  hasNoNullExceptAux t r (Array.length t) (Array.length t)


(**
   [sub v1 v2] returns [v1] - [v2]
*)
let sub t1 t2 =
  let t = Array.make (Array.length t1) Q.zero in
  for i = 0 to Array.length t - 1 do
    t.(i) <- Q.sub t1.(i) t2.(i)
  done;
  t


(**
   [scalar v1 v2 n] returns the scalar product of the first [n]
   dimensions of [v1] and [v2].
*)
let scalar v1 v2 =
  let s = ref Q.zero in
  let n = Array.length v1 in
  assert (n = Array.length v2) ;
  for i = 0 to n - 1 do
    s := Q.add !s (Q.mul v1.(i) v2.(i))
  done;
  !s

(** Operations on vector of terms. *)
module T = struct

  (** Create a vector *)
  let create typ x n =
    let f i = (Symbol.declare typ (x ^ string_of_int i)) in
    Array.init n f

  let term t = Array.map T.symbol t

  let symbol typ t = Array.map (Symbol.term typ) t

  let get_value ~model t = Array.map (Model.get_value ~model) t

  (**
     [scalar u l] returns the expression "u.l"
  *)
  let scalar_int f u l =
    assert (Array.length l = Array.length u) ;
    let f i = T.(f l.(i) * u.(i)) in
    T.add @@ BatList.init (Array.length u) f

  let scalar u l = scalar_int (fun x -> x) u l
  let scalar_q u l = scalar_int T.rat u l
  let scalar_z u l = scalar_int T.bigint u l

  let sum x = T.add @@ Array.to_list x

  let sub t1 t2 = BatArray.map2 T.(-) t1 t2

  let eq t1 t2 =
    let n = Array.length t1 in
    assert (n = Array.length t2) ;
    let rec aux i acc =
      if i >= n then acc
      else aux (i+1) (T.(t1.(i) = t2.(i)) :: acc)
    in
    T.and_ (aux 0 [])

  let neq t1 t2 =
    let n = Array.length t1 in
    assert (n = Array.length t2) ;
    let rec aux i acc =
      if i >= n then acc
      else aux (i+1) (T.(t1.(i) <> t2.(i)) :: acc)
    in
    T.and_ (aux 0 [])

  (** [is_diff u v1 v2] returns the expression ∀ n, u[n] = v1[n] - v2[n] *)
  let rec is_diff u v1 v2 =
    let rec aux n acc =
      if n >= 0 then
        aux (n - 1) T.( u.(n) = v1.(n) - v2.(n)  &&  acc)
      else
        acc
    in
    let l = Array.length u in
    assert (l = Array.length v1 && l = Array.length v2) ;
    aux (l-1) T.true_

  (**
     [unsatisfied_by_ranking u l] returns the expression "u.l <= 0"
  *)
  let unsatisfied_by_ranking u l =
    T.(scalar_q u l <= rat Q.zero)

  (**
     [is_null u] returns the expression u = 0
  *)
  let rec is_null u =
    let f t x =
      T.( x = T.rat Q.zero && t )
    in Array.fold_left f T.true_ u

  let rec is_ek_x v x j i =
    if i >= 0 then
      let j' = j + 1 in
      let i' = i - 1 in
      T.( T.symbol v.(j') = T.symbol x.(i) && is_ek_x v x j i')
    else
      T.true_

  let rec is_ek_0 v j i =
    if i >= 0 then
      let j' = j + 1 in
      let i' = i - 1 in
      T.( T.symbol v.(j') = T.rat Q.zero && is_ek_0 v j i')
    else
      T.true_

  (**
     [state_neq_implies k i r] returns the formula "k=i or r", which is
     equivalent to "k!=i => r" but does not use any negation (and is
     thus compatible with the generalize_model function)
  *)
  let state_neq_implies k i r =
    T.(k = T.rat (Q.of_int i) || r)

  let rec is_ek_aux v k x state states vars =
    if state >= 0 then
      mk_and
        (state_eq_implies k states state (is_ek_x v x (state*(vars+1)) vars))
        (mk_and
           (state_neq_implies k state (is_ek_0 v (state*(vars+1)) vars))
           (is_ek_aux v k x (state-1) states vars))
    else
      T.true_

  (**
     [is_ek v k x states vars] returns the expression v = e_k(x)
  *)
  let is_ek v k x states vars =
    is_ek_aux v k x states states vars


end
