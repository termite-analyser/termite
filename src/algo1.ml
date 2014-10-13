(**
   First (non-terminating) algorithm
*)

open Debug
open Invariants
open Smt
open ZZ

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
let algo1 ?(verbose=false) ?(timeout=10) block_dict var_dict invariant tau =

  let {Invariants. variables ; control_point } = invariant in

  let cons_I = Invariants.to_matrix invariant in

  (* The nb of invariants (and the size of the lambda). *)
  let m = Array.length cons_I in

  (* The nb of variables used by the polyhedron (and the size of the ranking function. *)
  let n = Array.length variables in

  (* If there are some invariants, the number of variable must be consistent. *)
  if m > 0 && Array.length cons_I.(0) <> n then raise (Invalid_argument "algo1: invariants") ;


  (* The [x], [x'] and [u = x - x'] smt elements. *)
  let f b i =
    let var :> znum term = try
        var_dict b variables.(i)
      (* Some variables are not present primed, so we just used the not primed version. *)
      with Llvm2smt.Variable_not_found (_,llv) when b ->
        Format.eprintf
          "Warning: Use of non-primed variable as placeholder for %s.\n%!"
          (Llvm.value_name llv) ;
        var_dict false variables.(i)
    in
    Symbol.term Num var in
  let x = Array.init n (f false) in
  let x' = Array.init n (f true) in
  let u =
    BatArray.map2
      (fun x x' -> Symbol.term Real T.( !x - !x' ))
      x x'
  in

  if verbose then begin
    Format.printf "cons_I: %a" pp_zmatrix cons_I ;
  end ;

  (* Problem that the SMT solver will solve : *)

  (* I /\ tau *)
  let rel = T.and_ [
      tau ;
      block_dict true control_point ;
      block_dict false control_point ;
    ] in

  let solver = Solver.make () in

  Solver.add ~solver rel ;
  Z3.Solver.push solver ;

  (* The ranking function, in two parts : the coefs l and the constant. *)
  let l = Array.make n Q.zero in
  let constant = ref Q.zero in

  let rec aux c timeout =

    if timeout = 0 then
      (* The algorithm has made too much iterations, terminate it *)
      raise Timeout;

    Z3.Solver.pop solver 1 ; (* Remove the previous assert *)
    Z3.Solver.push solver ; (* Put the backtrack point again. *)

    (* The problem that will be solved by the SMT solver *)
    let dir_u = T.(Vector.T.(scalar_q (term u) l) <= rat Q.zero) in
    Solver.add ~solver dir_u ;

    if verbose then Format.printf "Problem:@\n%s@\n" T.(to_string @@ simplify dir_u) ;

    match Solver.check ~solver [] with
      | Sat (lazy model) -> begin
          if verbose then Format.printf "Model: %s@\n" (Z3.Model.to_string model) ;
          (* The problem is satisfiable *)
          let u = Vector.T.get_value ~model u in
          let c = u::c in

          if verbose then
            begin
              Format.printf "x = %a@\n x' = %a@\n u = x - x' = %a@\n"
                pp_coefs (None, Vector.T.get_value ~model x  , Vector.T.term x )
                pp_coefs (None, Vector.T.get_value ~model x' , Vector.T.term x')
                pp_qarray u
            end;

          (* Solve the LP problem *)
          let (lambda, delta) = Lp.lp_one_control_point ~verbose c cons_I in

          if verbose then
            begin
              Format.printf "lambda = %a@." pp_qarray lambda;
              Format.printf "delta = %a@." pp_qarray delta;
            end;

          if Vector.isNull lambda then
            (* lambda is null, i.e. it isn't possible to have a better
               ranking function *)
            (l, !constant)
          else
            begin
              (* l <- sum_i=1..m lambda_j * l_j *)
              for k = 0 to n-1 do
                l.(k) <- Q.zero;
                for i = 0 to m-1 do
                  l.(k) <- Q.( l.(k) +  lambda.(i) * of_bigint cons_I.(i).(k) )
                done
              done;

              constant := Q.zero ;
              for i = 0 to m-1 do
                constant := Q.( !constant + lambda.(i) * of_bigint invariant.polyhedron.(i).constant )
              done ;

              if verbose then
                begin
                  Format.printf "l = %a@." pp_qarray l ;
                end;

              aux c (timeout-1)
            end
        end
      | _ ->
          (* l is a strict ranking function *)
          (l, !constant)
  in

  aux [] timeout
