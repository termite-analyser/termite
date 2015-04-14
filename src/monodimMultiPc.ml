(**
   Monodimensional algorithm for multiple control points
*)

open Debug
open Invariants
open Smt
open ZZ


(**
   [monodimensionalMultipc vars states inv smt_inv tau verbose] returns the linear
   component [l] of a weak ranking function, such that [pi(l)] is
   maximal.

   - [vars] is associating each variable of the automaton to a unique
   positive integer (see {!Automaton.indexed_strings})
   - [states] is associating each state of the automaton to a unique
   positive integer (see {!Automaton.indexed_strings})
   - [inv] contains the invariants (see {!Invariants.invariants})
   - [smt_inv] is the formula representing the invariant
   - [tau] is the formula representing the transition relation
   - If [verbose] is true, prints the values of [l], [x], [y], [x-y],
   [cost], [lambda] and [delta] at each iteration.
*)
let monodimensional ?(verbose=false) block_dict var_dict invariants tau =

  (** Perf markers *)
  let loops = ref 0 in
  let lp_max_size = ref (0,0) in
  let update_lp_max_size (a,b) =
    let (a',b') = !lp_max_size in
    if (a * b) > (a' * b') then lp_max_size := (a, b)
    else ()
  in
  let control_points = List.length invariants in

  let return result =
    { result ; loops = !loops ; lp_max_size = !lp_max_size ;
      control_points ;
    }
  in


  let variables, constants, cons_I = Invariants.group_to_matrix invariants in

  if verbose then begin
    Format.printf "Constraint(I) =@. %a@." pp_zmatrix cons_I
  end ;

  (* The nb of constraints (and the size of the lambda). *)
  let m = Array.length cons_I in

  (* The nb of variables used by the polyhedron (and the size of the ranking function. *)
  let n = Array.length variables in



  (** Equivalence between boolean and control points.
      ∀i, bᵢ <=> (k = i)
  *)
  let index_to_cp primed cp =
    T.and_ @@
    BatList.mapi
      (fun i inv ->
         T.( block_dict primed inv.control_point <=> (!cp = int i) )
      )
      invariants
  in

  (** Start and destination basic blocks of the trace. *)
  let cp = Symbol.declare Int "k" in
  let cp' = Symbol.declare Int "k'" in

  let max_cp = control_points - 1 in
  let is_cp cp = T.(int 0 <= !cp && !cp <= int max_cp) in

  let assert_cp =
    T.and_ [
      is_cp cp ;
      is_cp cp' ;
      index_to_cp false cp ;
      index_to_cp true cp' ;
    ]
  in


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
    Symbol.term Num var
  in

  let x  = Array.init n (f false) in
  let x' = Array.init n (f true) in

  (** eₖ(x) = the vector of size n with the components that correspond to the k-th
      control points equal to x and other all components equal to 0.
      ( 0 ----- 0 xᵢ ----- xⱼ  0 ------ 0 )
      ∀n, i ≤ n ≤ j, cp(xₙ) = k
  *)
  let var_to_cp = Invariants.get_var_to_cp invariants in
  let e k x =
    Array.mapi
      (fun i x_i ->
         match var_to_cp i with
           | Some cp_i -> T.(ite (int cp_i = !k) !x_i (int 0))
           | None -> assert false
      )
      x
  in

  let u = Vector.T.create Real "u" n in

  (* I /\ tau *)
  let rel = T.and_ [
      tau ;
      assert_cp ;
      Vector.T.(is_diff (term u) (e cp x) (e cp' x')) ; (* u = e^k(x) - e^k'(x') *)
    ] in


  (** Check if (x,x)∈τ, if it is, the function will never be strict. *)
  let has_no_fixpoint =
    let solver = Solver.make () in
    Solver.add ~solver T.(rel && Vector.T.(is_null @@ term u)) ;
    match Solver.check ~solver [] with
      | Unsat _ -> true
      | Sat (lazy model) -> begin
          if verbose then Format.printf "Fixpoint Model:@.%s@." (Z3.Model.to_string model) ;
          false
        end
      | Unkown _ -> assert false
  in

  (* Basis of a subspace of vectors u such that u.l = 0 for all
     ranking function l *)
  let b = Subspace.empty_subspace n in

  (* The ranking function *)
  let l = Array.make n Q.zero in
  let constant = ref Q.zero in


  let cost = Symbol.declare Real "cost" in

  (* Minimize u.l(<=0) : [cost + u.l = 0 && cost >= 0 ] *)
  let make_cost_formula l =
    T.(
      !cost >= int 0 &&
      !cost + Vector.T.(scalar_q (term u) l) = int 0 )
  in

  let solver = Optimize.make () in

  let rec aux c rays strict =

    incr loops ;

    let prob = T.and_ [
        rel ;
        make_cost_formula l ;
        Subspace.avoid_space b (Vector.T.term u) ;
      ] in

    if verbose then Format.printf "Problem:@.%s@." T.(to_string @@ simplify prob) ;
    Optimize.add ~solver prob ;
    let obj = Optimize.maximize ~solver T.(!cost) in

    match Optimize.check ~solver with
      | Sat (lazy model) -> begin
          if verbose then Format.printf "Model:@.%s@." (Z3.Model.to_string model) ;

          (* The problem is satisfiable *)
          let u_val = Vector.T.get_value ~model u in
          let c = u_val::c in

          if verbose then
            begin
              Format.printf "k = %a@. k' = %a@. u = %a@."
                Z.pp_print (Model.get_value ~model cp)
                Z.pp_print (Model.get_value ~model cp')
                pp_qarray u_val ;
            end;

          (* If an unbounded ray is found, add it to c *)
          let c, rays =
            if Optimize.get_upper ~solver obj |> T.symbol |> T.to_string = "oo"
            then c, rays (* Nothing to do *)
            else
              let cost_val = Model.get_value ~model cost in
              Optimize.add ~solver prob ;
              Optimize.add ~solver T.(!cost <= rat cost_val + int 1) ;
              let _objective = Optimize.maximize ~solver T.(!cost) in
              let ray =
                match Optimize.check ~solver with
                  | Sat (lazy model) -> begin
                      let v = Vector.T.get_value ~model u in
                      let ray = Vector.sub v u_val in

                      if verbose then
                        begin
                          Format.printf "ray = %a@." pp_qarray ray ;
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
            (* lambda is null (and so is delta), i.e. it isn't possible
               to have a better ranking function *)
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
                constant := Q.( !constant + lambda.(i) * of_bigint constants.(i) )
              done ;

              Subspace.add_to_subspace b u_val ;

              if verbose then
                begin
                  Format.printf "Current subspace: @.%a@." Subspace.pp b;
                  Format.printf "l = %a@." pp_qarray l;
                  Format.printf "------@.";
                end;

              aux c rays (Vector.hasNoNullExcept delta rays)
            end
        end
      | _ -> return (l, !constant, has_no_fixpoint && strict)
  in

  aux [] [] false
