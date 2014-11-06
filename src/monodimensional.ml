(**
   Monodimensional algorithm
*)

open Debug
open Invariants
open Smt
open ZZ




(**
   [monodimensional vars inv smt_inv tau verbose] returns the linear
   component [l] of a weak ranking function, such that [pi(l)] is
   maximal.

   - [vars] is associating each variable of the automaton to a unique
   positive integer (see {!Automaton.indexed_strings})
   - [inv] contains the invariants (see {!Invariants.invariants})
   - [smt_inv] is the formula representing the invariant
   - [tau] is the formula representing the transition relation
   - If [verbose] is true, prints the values of [l], [x], [y], [x-y],
   [cost], [lambda] and [delta] at each iteration.
*)
let monodimensional ?u ?(verbose=false) block_dict var_dict invariant tau =

  (** Perf markers *)
  let loops = ref 0 in
  let lp_max_size = ref (0,0) in
  let update_lp_max_size (a,b) =
    let (a',b') = !lp_max_size in
    if (a * b) > (a' * b') then lp_max_size := (a, b)
    else ()
  in
  let control_points = 1 in

  let return result =
    { result ; loops = !loops ; lp_max_size = !lp_max_size ;
      control_points ;
    }
  in


  let {Invariants. variables ; control_point } = invariant in

  let cons_I = Invariants.to_matrix invariant in

  (* The nb of invariants (and the size of the lambda). *)
  let m = Array.length cons_I in

  (* The nb of variables used by the polyhedron (and the size of the ranking function. *)
  let n = Array.length variables in

  (* If there are some invariants, the number of variable must be consistent. *)
  if m > 0 && Array.length cons_I.(0) <> n then raise (Invalid_argument "algo1: invariants") ;

  (* The [u = x - x'] smt element. *)
  let u = match u with
    | Some u -> u
    | None -> begin
        let f b i =
          let var :> znum term = try
              var_dict b variables.(i)
            (* Some variables are not present primed, so we just used the not primed version. *)
            with Llvm2smt.Variable_not_found (_,llv) when b ->
              Format.eprintf
                "Warning: Use of non-primed variable as placeholder for %s.@."
                (Llvm.value_name llv) ;
              var_dict false variables.(i)
          in
          Symbol.term Real var in
        let x  = Array.init n (f false) in
        let x' = Array.init n (f true) in
        BatArray.map2
          (fun x x' -> Symbol.term Real T.( !x - !x' ))
          x x'
      end
  in

  if verbose then begin
    Format.printf "cons_I: %a@." pp_zmatrix cons_I ;
  end ;


  (* Problem that the SMT solver will solve : *)

  (* I /\ tau *)
  let rel = T.and_ [
      tau ;
      block_dict true control_point ;
      block_dict false control_point ;
    ] in

  (* Basis of a subspace of vectors u such that u.l = 0 for all
     ranking function l *)
  let b = Subspace.empty_subspace n in

  (* The ranking function, in two parts : the coefs l and the constant. *)
  let l = Array.make n Q.zero in
  let constant = ref Q.zero in

  let cost = Symbol.declare Real "cost" in

  (* Minimize u.l : [cost + u.l = 0] *)
  let make_cost_formula l =
    T.(!cost + Vector.T.(scalar_q (term u) l) = int 0 )
  in

  let solver = Solver.make () in

  let rec aux c rays strict =
    incr loops ;

    let av_space = Subspace.avoid_space b (Vector.T.term u) in

    let prob = T.and_ [
        rel;
        make_cost_formula l ;
        av_space ;
      ] in

    if verbose then Format.printf "Problem:@.%s@." T.(to_string prob) ;

    if verbose then Format.printf "Avoid Space:@.%s@." T.(to_string av_space) ;
    Solver.add ~solver prob ;
    (* if verbose then *)
    (*   Format.printf "smt-solve a pb of size (#or + #and) %d @." (Smt.size_of_formula prob); *)

    match Opti.check ~solver T.(!cost) true with
      | Sat (lazy model), unbounded -> begin
          if verbose then Format.printf "Model: %s@." (Z3.Model.to_string model) ;
          (* The problem is satisfiable *)
          let u_val = Vector.T.get_value ~model u in
          let c = u_val::c in

          if verbose then
            begin
              Format.printf "u = %a@." pp_qarray u_val;
            end;

          (* If an unbounded ray is found, add it to c *)
          let c, rays =
            if not unbounded then c, rays (* Nothing to do *)
            else
              let cost_val = Model.get_value ~model cost in
              Solver.add ~solver prob ;
              Solver.add ~solver T.(!cost <= rat cost_val + int 1) ;
              let ray =
                match Opti.check ~solver T.(!cost) true with
                  | Sat (lazy model), true -> begin
                      let v = Vector.T.get_value ~model u in
                      let ray = Vector.sub v u_val in

                      if verbose then
                        begin
                          Format.printf "ray = %a@." pp_qarray ray
                        end;

                      ray
                    end
                  | _ -> assert false
                    (* It was sat before, it shouldn't be suddenly unsat.
                       And since it will be bounded, it should optimize. *)
              in
              ray::c, (List.length c+1)::rays
          in

          (* Solve the LP problem *)
          let (lambda, delta) = Lp.lp_one_control_point ~verbose c cons_I in
          update_lp_max_size (Array.length lambda, Array.length delta) ;

          if verbose then
            begin
              Format.printf "lambda = %a@." pp_qarray lambda;
              Format.printf "delta = %a@." pp_qarray delta;
            end;

          if Vector.isNull lambda then
            (* lambda is null, i.e. it isn't possible to have a better
               ranking function *)
            return (l, !constant, false)
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

              Subspace.add_to_subspace b u_val ;

              if verbose then
                begin
                  Format.printf "Current subspace: @.%a@." Subspace.pp b;
                  Format.printf "l = %a@." pp_qarray l;
                  Format.printf "------@.@?";
                end;

              aux c rays (Vector.hasNoNullExcept delta rays)
            end
        end
      | _ -> return (l, !constant, strict)
  in

  aux [] [] false
