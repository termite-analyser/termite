(**
   Multidimensional algorithm
*)

open Debug
open Invariants
open Smt
open ZZ
open Monodimensional

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
let multidimensional ?(verbose=false) block_dict var_dict invariant tau =

  let {Invariants. variables } = invariant in

  (* The nb of variables used by the polyhedron (and the size of the ranking function. *)
  let n = Array.length variables in

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
  let u =
    BatArray.map2
      (fun x x' -> Symbol.term Real T.( !x - !x' ))
      x x'
  in

  (* Basis of Span(rho) *)
  let rho_basis = Subspace.empty_subspace n in

  let rec aux rho tau =
    if verbose then
      begin
        Format.printf "-- Dimension %i ---@." (List.length rho);
      end;

    (* Find a monodimensional weak ranking function *)
    let l, offset, s = monodimensional ~u ~verbose block_dict var_dict invariant tau in

    if s then
      (* If l is strict, then l::rho is a strict multidimensional
         ranking function : return it *)
      Some ((l, offset)::rho)
    else if (Subspace.is_in rho_basis l) then
      (* l is in Span(rho) i.e. rho cannot satisfy the remaining
         transitions *)
      None
    else
      begin
        (* Add l to the basis *)
        Subspace.add_to_subspace rho_basis l;

        (* Remove the satisfied transitions from tau and
           reiterate *)
        aux ((l, offset)::rho) T.(tau && Vector.T.(scalar_q (term u) l) = rat Q.zero)
      end
  in
  aux [] tau
