(*Main file for test. only one file for all algorithms*)

open Debug
open Llvm
open Smt

module SMTg = Smt_graph.Make (Smt.ZZ)
module Llvm2Smt = Llvm2smt.Init (Smt.ZZ) (SMTg)

let print s =
  (if !Config.debug then Printf.fprintf else Printf.ifprintf) stdout s

type config_t = {
  inputfile:string;
  algotype:int;
}

(** Read the bitcode, output a list of llvm functions. *)
let read_bitcode file =
  print "Reading %s\n%!" file ;
  let ctx = Llvm.create_context () in

  (* Parse the bitcode. *)
  let mem = Llvm.MemoryBuffer.of_file file in
  let m = Llvm_bitreader.parse_bitcode ctx mem in
  Llvm.MemoryBuffer.dispose mem ;

  (* Apply the mem2reg pass, just in case. *)
  let pass = PassManager.create () in
  Llvm_scalar_opts.add_memory_to_register_promotion pass ;
  ignore @@ PassManager.run_module m pass ;

  Llvm.fold_left_functions (fun l x -> x :: l) [] m


let get_invariants_of_cpoint invariants cpoint =
  try List.find (fun b -> b.Invariants.control_point = cpoint) invariants
  with Not_found ->
    failwith
      (Printf.sprintf "Couldn't find invariants for the block %s."
         (value_name @@ value_of_block cpoint))


(** Transform a bitcode straight to an smt formula. *)
let llvm2smt llfun =
  print "Transforming %s into an smt formula.\n%!" (Llvm.value_name llfun) ;
  let open Llvm2Smt in
  let open Llvm_graph in

  (* Get the graph *)
  let llb2node, llg = of_llfunction llfun in

  (* Get pagai's control points *)
  let cpoints = Invariants.(get_pagai_control_points @@ get_invariant_metadatas llfun) in
  print "%i control points:" @@ List.length cpoints ;
  List.iter (fun b -> print " %s" @@ string_of_llvalue @@ value_of_block b) cpoints ;
  print "\n%!" ;

  (* Get pagai's invariants. *)
  let invariants = Invariants.from_llfun llfun in

  (* Filter out the invariants to get only the control_points. *)
  let cp_invariants = List.map (get_invariants_of_cpoint invariants) cpoints in

  (* Break down the graph. *)
  let llg' = break_list llg @@ basicblocks_to_vertices llg cpoints in
  if !Config.debug then begin
    let file = open_out (value_name llfun ^ ".term.dot") in
    Dot.output_graph file llg' ;
    close_out file ;
  end ;

  (* Transform the CFG to SMT. *)
  let smtg = llvm2smt llfun cpoints llg' in
  if !Config.debug then begin
    let file = open_out (value_name llfun ^ ".term.smt.dot") in
    SMTg.Dot.output_graph file smtg ;
    close_out file ;
  end ;
  let smt_cfg = SMTg.to_smt smtg in
  print "CFG Formula:\n%s\n%!" ZZ.T.(to_string smt_cfg) ;

  let encode_block inv =
    ZZ.T.imply
      (get_block false inv.Invariants.control_point)
      (Invariants.to_smt (get_var false) inv)
  in
  let smt_inv = ZZ.T.and_ @@ List.map encode_block invariants in
  print "Invariants Formula:\n%s\n%!" ZZ.T.(to_string smt_inv) ;

  (* The end. *)
  let smt = ZZ.T.and_ [smt_cfg ; smt_inv] in
  smt, cp_invariants


let get_unique_invariant ~llf = function
  | [] -> failwith @@ Printf.sprintf "No invariants for function %s." @@ value_name llf
  | [ cp ] -> cp
  | _ -> failwith @@
      Printf.sprintf "Algo 1, 2 and 3 only accept function with one control points. %s has multiple control points" @@ value_name llf

(** Do I really need to tell you what it's doing ? :) *)
let compute_ranking_function ~llf algo block_dict dictionnary invariants tau =
  print "Computing ranking functions with algorithm n°%d \n%!" algo ;
  match algo with
    | 1 ->
        let inv = get_unique_invariant ~llf invariants in
        let res =
          Algo1.algo1 ~verbose:!Config.debug block_dict dictionnary inv tau
        in
        let l, c = res.result in
        let vars = Array.map (dictionnary false) inv.variables in
        {res with result = [ (l, c, vars, true) ]}
    | 2 ->
        let inv = get_unique_invariant ~llf invariants in
        let res =
          Monodimensional.monodimensional
            ~verbose:!Config.debug block_dict dictionnary inv tau
        in
        let l, c, b = res.result in
        let vars = Array.map (dictionnary false) inv.variables in
        {res with result = [ (l, c, vars, b) ]}
    | 3 ->
        let inv = get_unique_invariant ~llf invariants in
        let res =
          Multidimensional.multidimensional
            ~verbose:!Config.debug block_dict dictionnary inv tau in
        let vars =  Array.map (dictionnary false) inv.variables in
        let result = List.map (fun (l,c,b) -> (l,c, vars, b)) res.result
        in
        {res with result }
    | 4 ->
        let res = MonodimMultiPc.monodimensional ~verbose:!Config.debug block_dict dictionnary invariants tau in
        let l, c, b = res.result in
        let vars, _, _ = Invariants.group_to_matrix invariants in
        {res with result = [ (l, c, Array.map (dictionnary false) vars, b) ]}
    | n -> failwith (Printf.sprintf "There is no algorithm n°%i" n)


let print_result fmt res =
  let pp_strict fmt b =
    Format.pp_print_string fmt (if b then "strict" else "not strict")
  in
  let pp fmt (l,c, vars, strict) =
    Format.fprintf fmt "l = %a@.The ranking function is %a"
      pp_ranking_fun (c, l, vars)
      pp_strict strict
  in
  pp_result (Format.pp_print_list pp) fmt res

let do_analysis config =
  let time = Unix.gettimeofday () in

  config.inputfile
  |> read_bitcode
  |> List.iter (fun llfun ->
    if Array.length (Llvm.basic_blocks llfun) = 0
    then () (* If the function is empty, we just skip it. *)
    else begin
      let tau, invariants = llvm2smt llfun in
      let res =
        compute_ranking_function
          llfun config.algotype
          Llvm2Smt.get_block Llvm2Smt.get_var
          invariants tau
      in

      (* Shouldn't be here, just a shortcut to print stuff *)
      print_result Format.std_formatter res
    end) ;

  let new_time = Unix.gettimeofday () in
  Format.printf "@.This analysis took %f seconds.@." (new_time -. time)




exception NotFound of string

let set_numalgo config nb= config := {!config with algotype=nb}
let make_default_config () = {inputfile = "" ; algotype = 4 }
let set_and_verify_file config (s:string) =
  if not (Sys.file_exists s)  then  raise (NotFound s)
  else
      config := {!config with inputfile=s}


let read_args  () =
  let cf = ref (make_default_config ()) in
  let speclist = [
    "--version",
    Arg.Unit (fun () ->
      Printf.printf "Termite Version %s \n%!" Config.version; exit 0),
    ": print version and exit" ;

    "-algo",
    Arg.Int (set_numalgo cf),
    ": Algo1(1),mono(2), multi(3) or multipc(4). Default is 4." ;

    "-v", Arg.Unit (fun () -> Config.debug := true),": Be verbose." ;
  ]
  in
  let usage_msg = "Usage : termite [options] file" in

  Arg.parse speclist (set_and_verify_file cf) usage_msg ;
    if !cf.inputfile = "" then begin Arg.usage speclist usage_msg ; exit 1 end;
    !cf
(*end of options*)


(*Main function*)

let _ =
  try
    let cf = read_args () in
    do_analysis cf
  with
    | NotFound s ->
        Printf.eprintf "File not found: %s\n%!" s ; exit 1
    | Sys_error s ->
        Printf.eprintf "Sys_error: %s\n%!" s ; exit 1
    | Llvm2smt.Not_implemented llv ->
        Printf.eprintf "%s\n%!" @@ Llvm2smt.sprint_exn llv ; exit 1
    | Llvm2smt.Variable_not_found x as exn ->
        Printf.eprintf "%s\n%!" @@ Llvm2smt.sprint_exn_var x ; raise exn
    | Llvm2smt.Block_not_found x as exn ->
        Printf.eprintf "%s\n%!" @@ Llvm2smt.sprint_exn_block x ; raise exn
