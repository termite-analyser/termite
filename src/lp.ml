(** Module used for LP solving. *)

open Smt
open ZZ

(** [all_pos vec] ≡ ∀x∈vec 0≤x *)
let all_pos x =
  let zero : znum term = T.int 0 in

  let f i = T.(zero <= x.(i)) in

  T.and_ @@ BatList.init (Array.length x) f


(** [between_0_1 vec] ≡ ∀x∈vec 0≤x≤1 *)
let between_0_1 x =
  let zero : znum term = T.int 0 in
  let one : znum term = T.int 1 in

  let f i = T.(zero <= x.(i) && x.(i) <= one) in

  T.and_ @@ BatList.init (Array.length x) f

(** Let mat = [make_lp_matrix v a], then [x.(i).(j) = vⱼ.aᵢ]. *)
let make_lp_matrix v a =
  let n = List.length v in
  let m = Array.length a in
  let mat = Array.make_matrix m n Q.zero in
  for i = 0 to m - 1 do
    let a_ = Array.map Q.of_bigint a.(i) in
    List.iteri (fun j v_ -> mat.(i).(j) <- Vector.scalar v_ a_) v
  done ;
  mat


(** [compose_polyhedron v a λ δ] ≡ ∀j=1..N ( Σi=1..m   λᵢ*(vⱼ.aᵢ) ≥ δⱼ )
    with N = |v| = |δ| and m = |a| = |λ|
*)
let compose_polyhedron mat lambda delta =
  let n = Array.length delta in
  let m = Array.length lambda in
  assert (m = Array.length mat && (m = 0 || n = Array.length mat.(0))) ;

  (* [gen_cell j i] ≡ λᵢ*(vⱼ.aᵢ) *)
  let gen_cell j i =
    T.(lambda.(i) * T.rat mat.(i).(j)) in

  (* [gen_ineq j] ≡ Σi=1..m [gen_cell j i] ≥ δⱼ *)
  let gen_ineq j = T.( add @@ BatList.init m (gen_cell j) >= delta.(j) ) in

  (* ∀j=1..N [gen_ineq j] *)
  T.and_ (BatList.init n gen_ineq)



(** Encode the LP problem with the given set of variables *)
let lp_given_variables mat lambda delta =
  T.and_ [
    all_pos lambda ;
    between_0_1 delta ;
    compose_polyhedron mat lambda delta
  ]



let pp_lp_matrix fmt mat =
  let m = Array.length mat in
  if m = 0 then ()
  else begin
    let n = Array.length mat.(0) in
    for j = 0 to n - 1 do
      Format.fprintf fmt "%a * λ_%i" Q.pp_print mat.(0).(j) 0 ;
      if m > 1 then
        for i = 1 to m - 1 do
          Format.fprintf fmt " + %a * λ_%i" Q.pp_print mat.(i).(j) i
        done ;
      Format.fprintf fmt " ≥ δ_%i@," j ;
    done ;
  end

(** [lp_one_control_point v a] solves LP problem when there is only one control point.
    [v] is the convex hull of the polyhedron.
    [a], or Cons_I is the set of constraints of the invariant.
    @return (lambda, delta).
*)
let lp_one_control_point ?(verbose=false) v a =
  let n = List.length v in
  let m = Array.length a in
  let delta = Vector.T.create Real "δ" n in
  let lambda = Vector.T.create Real "λ" m in
  let cost = T.symbol @@ Symbol.declare Real "cost" in

  let delta' = Vector.T.term delta in
  let lambda' = Vector.T.term lambda in

  let cost_smt = T.( cost = Vector.T.sum delta') in
  let mat = make_lp_matrix v a in
  let smt = lp_given_variables mat lambda' delta' in

  (* Hack, to convince optiz3 that cost is indeed bounded. *)
  let cost_bound = T.( cost <= int n) in

  if verbose then begin
    Format.printf "Lp: @[%a@]@." pp_lp_matrix mat
  end ;

  let solver = Optimize.make () in
  Optimize.add ~solver smt ;
  Optimize.add ~solver cost_smt ;
  Optimize.add ~solver cost_bound ;
  let _obj = Optimize.maximize ~solver cost in
  let time = Unix.gettimeofday () in
  let solution = Optimize.check ~solver in
  let time = Unix.gettimeofday () -. time in
  if verbose then Format.printf "LP problem solved in %f seconds.@." time ;
  match solution with
    | Sat (lazy model) ->
        let lambda_res = Vector.T.get_value ~model lambda in
        let delta_res = Vector.T.get_value ~model delta in
        lambda_res, delta_res
    | _ -> assert false
