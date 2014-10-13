
open Llvm
open Smt.ZZ

type rel = Ge | Gt | Eq
let rel_of_string = function
  | ">=" -> Ge
  | ">" -> Gt
  | "==" -> Eq
  | _ -> assert false
let rel_to_smt = function
  | Ge -> T.ge
  | Gt -> T.gt
  | Eq -> T.eq

type face = {
  rel : rel ;
  constant : Z.t ;
  coefs : Z.t array ;
}

type invariant = {
  control_point : llbasicblock ;
  variables : llvalue array ;
  polyhedron : face array ;
}



(* Transform into the matrix of coeficients. Simply extract the matrix of coefs. *)
let to_matrix inv = Array.map (fun f -> f.coefs) inv.polyhedron

(** Take a list of matrices and make a big ""diagonal"" matrix with it. *)
let block_diag l =
  let n = BatList.sum (List.map Array.length l) in
  let m = BatList.sum (List.map
        (fun a -> if Array.length a > 0 then Array.length a.(0) else 0)
        l) in
  let mat = Array.make_matrix n m Z.zero in
  let kn = ref 0 in
  let km = ref 0 in
  let aux a =
    let an = Array.length a in
    if an > 0 then
      let am = Array.length a.(0) in
      for i = 0 to an - 1 do
        for j = 0 to am - 1 do
          mat.(!kn + i).(!km + j) <- a.(i).(j)
        done
      done ;
      kn := !kn + an ;
      km := !km + am ;
    else ()
  in
  List.iter aux l ;
  mat

(* Same as {!to_matrix} but with a list of invariants. *)
let group_to_matrix invs =
  let vars = Array.concat (List.map (fun i -> i.variables) invs) in
  let constants =
    Array.concat @@
    List.map
      (fun inv -> Array.map (fun f -> f.constant) inv.polyhedron)
      invs
  in
  let invs' =
    List.map (fun inv -> Array.map (fun f -> f.coefs) inv.polyhedron)
      invs
  in
  vars, constants, block_diag invs'

let get_var_to_cp invariants =
  let l = List.fold_right (fun inv l -> Array.length inv.variables :: l) invariants [] in
  let rec aux var_idx i = function
    | [] -> None
    | n :: t when var_idx < n -> Some i
    | n :: t -> aux (var_idx-n) (i+1) t
  in fun x -> aux x 0 l

(* Let p the polyhedron, v the variables.
   n the number of variables, m the number of side of the polyhedron.
   /\i=1..m (scalar i.(i) vars <= 0)
   /\i=1..m (sum_j=1..n (p.(i).(j) * v.(j)) <= 0)
*)
let to_smt dictionnary { variables = v ; polyhedron = p } =
  let n = Array.length v in
  let m = Array.length p in
  (* Small type cast here, to allow dictionnary to return any znum term. *)
  let f coefs j =
    T.(bigint coefs.(j) * (dictionnary v.(j) :> znum term)) in
  let g i =
    let { rel ; constant ; coefs } = p.(i) in
    let ($) = rel_to_smt rel in
    let l = T.bigint constant :: BatList.init n (f coefs) in
    T.( add l $ int 0) in
  T.and_ (BatList.init m g)



(** Pagai Interface *)

(** Collect all the variables than can be in an invariant.

    In order to do that :
    - We go through all the instructions to find the !pagai.var annotations.
    - We add the function parameters.
*)
let get_vars llf =
  let llc = module_context @@ global_parent llf in
  let pagai_var = mdkind_id llc "pagai.var" in

  let vars = Hashtbl.create 16 in
  let get_var llinstr =
    begin match metadata llinstr pagai_var with
      | None -> ()
      | Some metadata ->
          Hashtbl.add vars metadata llinstr
    end ;
    let s = value_name llinstr in
    if s <> "" then begin Hashtbl.add vars (mdstring llc s) llinstr
    end
  in
  iter_blocks (iter_instrs get_var) llf ;
  iter_params (fun llp -> Hashtbl.add vars (mdstring llc (value_name llp)) llp) llf ;

  vars

(** Collect all the pagai.invariant metadatas. *)
let get_invariant_metadatas llf =
  let llc = module_context @@ global_parent llf in
  let pagai_invariant = mdkind_id llc "pagai.invariant" in

  let get_invariants_root lli =
    let md = metadata lli pagai_invariant in
    match BatOption.bind md (fun md -> get_mdstring @@ operand md 0) with
      | Some "true" -> None
      | Some _ -> assert false
      | None -> md
  in

  let get_invariants_from_basicblock llb =
    let aux invars lli =
      match get_invariants_root lli with
        | None -> invars
        | Some md -> md :: invars
    in
    fold_left_instrs aux [] llb
  in
  fold_left_blocks (fun l b -> (b, get_invariants_from_basicblock b) :: l) [] llf

(** Extract invariants from a metada pagai.invariant.
    Returns the polyhedron and the list of variables involved.
*)
let extract_invariants vars md =
  let string_operand v i = BatOption.get @@ get_mdstring @@ operand v i in
  let v = operand md 0 in
  let ineqs = operand md 1 in
  let variables =
    Array.init (num_operands v)
      (fun i ->
         let llv = operand (operand v i) 0 in
         try Hashtbl.find vars llv
         with Not_found ->
           Printf.eprintf "Couldn't find this operand: %s\n%!" (string_of_llvalue llv) ;
           raise Not_found
      )
  in

  let aux_poly i =
    let ineq = operand ineqs i in
    let rel = rel_of_string @@ string_operand ineq 1 in
    let coefs_raw = operand ineq 0 in
    let get_coef i =
      Z.of_string @@ string_operand coefs_raw i
    in
    let constant = get_coef 0 in
    let coefs =
      Array.init (num_operands coefs_raw - 1) (fun i -> get_coef (i+1)) in
    {rel ; constant ; coefs }
  in
  let poly = BatList.init (num_operands ineqs) aux_poly in

  (variables, poly)

(* Will iterate on the llvm function and extract pagai's invariants. *)
let from_llfun llf =

  let vars = get_vars llf in

  let invars = get_invariant_metadatas llf in

  let aux_bb invariants (llb , llmds) =
    let aux invars llmd =
      let var', poly = extract_invariants vars llmd in
      match invars with
        | Some (var, polylist) -> begin
            assert (var = var') ;
            Some (var, poly @ polylist)
          end
        | None -> Some (var', poly)
    in
    match List.fold_left aux None llmds with
      | None -> invariants
      | Some (variables, p) ->
          { variables ; polyhedron = Array.of_list p ; control_point = llb } :: invariants
  in
  List.fold_left aux_bb [] invars

(** Compute control points according to pagai, from association list from basicbloc to invariant metadatas.

    The control points, according to pagai, are all the places where there are some invariants,
    except the first node (no predecessor) and the return node (no successor).
*)
let get_pagai_control_points invar_mds =
  let has_pred llb = Llvm_graph.predecessors llb <> [] in

  let aux l (llb, invs) =
    if invs <> [] && has_pred llb && Llvm_graph.has_successor llb then llb :: l
    else l
  in

  List.fold_left aux [] invar_mds
