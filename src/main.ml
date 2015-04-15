open Debug
open Llvm
open Smt

module SMTg = Smtgraph.Make (Smt.ZZ)
module Llvm2Smt = Llvm2smt.Init (Smt.ZZ) (SMTg)


type algos =
  | Algo1
  | Monodimensional
  | Multidimensional
  | MonodimMultiPc

let algo_to_string = function
  | Algo1  -> "Algo1"
  | Monodimensional  -> "Monodimensional"
  | Multidimensional  -> "Multidimensional"
  | MonodimMultiPc  -> "Monodimensional, Multiple control points"

type file =
  | C_file of string
  | BC_file of string

let print s =
  (if !Config.debug then Printf.fprintf else Printf.ifprintf) stdout s

exception External_error of string * string * int

(** Read the bitcode, output a list of llvm functions. *)
let read_bitcode pagai pagai_o clang clang_o file =
  let file = match file with
    | BC_file s -> s
    | C_file c_file ->
      print "Compiling C file %s to llvm bytecode.\n%!" c_file ;
      let make_file base suffix = let open Filename in
        concat (get_temp_dir_name ()) ((basename @@ chop_extension base) ^ suffix)
      in
      let clang_file = make_file c_file ".bc" in
      let clang_log  = make_file c_file ".clang.log" in
      let pagai_file = make_file c_file ".pagai.bc" in
      let pagai_log  = make_file c_file ".pagai.log" in

      let clang_command =
        Printf.sprintf "%s %s -c -emit-llvm -o %s %s &> %s"
          clang clang_o clang_file c_file clang_log
      in
      let clang_ret = Sys.command clang_command in
      if clang_ret <> 0 then raise @@ External_error(clang_command, clang_log, clang_ret) ;

      let pagai_command =
        Printf.sprintf "%s %s -b %s -i %s &> %s"
          pagai pagai_o pagai_file clang_file pagai_log
      in
      let pagai_ret = Sys.command pagai_command in
      if pagai_ret <> 0 then raise @@ External_error(pagai_command, pagai_log, pagai_ret) ;

      pagai_file
  in

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
  let module L = Llvmcfg in

  (* Get the graph *)
  let llb2node, llg = L.of_llfunction llfun in

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
  let llg' = L.break_by_list llg @@ L.basicblocks_to_vertices llg cpoints in
  if !Config.debug then begin
    let file = open_out (value_name llfun ^ ".term.dot") in
    L.Dot.output_graph file llg' ;
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
      (Invariants.to_smt (get_var ~primed:false) inv)
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
  print "Computing ranking functions with the algorithm: %s \n%!" (algo_to_string algo) ;
  match algo with
    | Algo1 ->
        let inv = get_unique_invariant ~llf invariants in
        let res =
          Algo1.algo1 ~verbose:!Config.debug block_dict dictionnary inv tau
        in
        let l, c = res.result in
        let vars = Array.map (dictionnary false) inv.variables in
        {res with result = [ (l, c, vars, true) ]}
    | Monodimensional ->
        let inv = get_unique_invariant ~llf invariants in
        let res =
          Monodimensional.monodimensional
            ~verbose:!Config.debug block_dict dictionnary inv tau
        in
        let l, c, b = res.result in
        let vars = Array.map (dictionnary false) inv.variables in
        {res with result = [ (l, c, vars, b) ]}
    | Multidimensional ->
        let inv = get_unique_invariant ~llf invariants in
        let res =
          Multidimensional.multidimensional
            ~verbose:!Config.debug block_dict dictionnary inv tau in
        let vars =  Array.map (dictionnary false) inv.variables in
        let result = List.map (fun (l,c,b) -> (l,c, vars, b)) res.result
        in
        {res with result }
    | MonodimMultiPc ->
        let res = MonodimMultiPc.monodimensional ~verbose:!Config.debug block_dict dictionnary invariants tau in
        let l, c, b = res.result in
        let vars, _, _ = Invariants.group_to_matrix invariants in
        {res with result = [ (l, c, Array.map (dictionnary false) vars, b) ]}


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

let do_analysis pagai pagai_o clang clang_o algo file =
  let time = Unix.gettimeofday () in
  let nb_fun = ref 0 in

  let results =
    file
    |> read_bitcode pagai pagai_o clang clang_o
    |> BatList.filter_map (fun llfun ->
      if Array.length (Llvm.basic_blocks llfun) = 0
      then None (* If the function is empty, we just skip it. *)
      else begin
        let tau, invariants = llvm2smt llfun in
        if List.length invariants = 0 then None
        else begin
          incr nb_fun ;
          let res =
            compute_ranking_function
              llfun algo
              (fun primed -> Llvm2Smt.get_block ~primed)
              (fun primed -> Llvm2Smt.get_var ~primed)
              invariants tau
          in
          Some (llfun, res)
        end
      end)
  in

  let all_strict =
    List.fold_left
      (fun b (_,{result}) -> List.fold_left (fun x (_,_,_,b) -> x && b) b result)
      true results
  in

  let new_time = Unix.gettimeofday () in

  if not !Config.quiet then begin
    Format.pp_print_list
      (fun fmt (llfun, res) ->
         Format.fprintf fmt "@.--- %s ----@." (Llvm.value_name llfun) ;
         print_result fmt res
      ) Format.std_formatter
      results ;
    Format.print_newline () ;
    Format.printf "%i functions were analyzed.@." !nb_fun ;
    Format.printf "This analysis took %f seconds.@." (new_time -. time) ;
  end
  else begin
    if all_strict then print_string "YES"
    else print_string "NO" ;
    Format.printf " %f@." (new_time -. time) ;
  end ;

  all_strict

open Cmdliner

let debug_t =
  let doc = "Print extra debugging information." in
  Arg.(value & flag & info ["d";"debug"] ~doc)

let quiet_t =
  let doc = "Print a compressed answer." in
  Arg.(value & flag & info ["q";"quiet"] ~doc)

let algo_t =
  let algos = [ "1", Algo1 ; "2", Monodimensional ;
                "3", Multidimensional ; "4", MonodimMultiPc ] in
  let doc =
    "Which algorithm. $(docv) must be one of \
     1 (Algo1), 2 (mono), 3 (multi) or 4 (multipc)." in
  Arg.(value & opt (enum algos) MonodimMultiPc & info ["algo"] ~docv:"ALGO" ~doc)

let file_t =
  let doc =
    "File processed by termite. \
     If it's a .c file, clang and pagai will be called on it to produce a llvm bitcode. \
     Otherwise, if it's a .bc file, it is assumed to have been already preprocessed by pagai."
  in
  let c_or_bc_file =
    let pa, pp = Arg.non_dir_file in
    let pa s = match pa s with
      | `Ok s when Filename.check_suffix s ".c"  -> `Ok (C_file s)
      | `Ok s when Filename.check_suffix s ".bc" -> `Ok (BC_file s)
      | `Ok s -> `Error (Arg.doc_quote s ^" is neither a .c file nor a .bc file")
      | `Error x -> `Error x
    in
    let pp fmt (C_file s | BC_file s) = pp fmt s in
    (pa, pp)
  in
  Arg.(required & pos 0 (some c_or_bc_file) None & info [] ~doc ~docv:"FILE")

let pagai_t =
  let doc = "Path to the pagai executable." in
  Arg.(value & opt string "pagai" & info ["pagai"] ~doc)

let pagai_opt_t =
  let doc = "Pagai options." in
  Arg.(value & opt string "" & info ["pagai-opt"] ~doc)

let clang_t =
  let doc = "Path to the clang executable." in
  Arg.(value & opt string "clang" & info ["clang"] ~doc)

let clang_opt_t =
  let doc = "Clang options." in
  Arg.(value & opt string "" & info ["clang-opt"] ~doc)

let ret_error f = Printf.ksprintf (fun s -> `Error (false, s)) f

let termite_t debug quiet pagai pagai_o clang clang_o algo file =
  Config.debug := debug ;
  Config.quiet := quiet ;
  try `Ok (do_analysis pagai pagai_o clang clang_o algo file)
  with
    | Sys_error s ->
      ret_error "System error: %s\n%!" s
    | Llvm2smt.Not_implemented llv ->
      ret_error "%s\n%!" @@ Llvm2smt.sprint_exn llv
    | External_error (cmd, log, code) ->
      ret_error "\"%s\" failed with error %i. See %s for details." cmd code log
    | Llvm2smt.Variable_not_found x as exn ->
      Printf.eprintf "%s\n%!" @@ Llvm2smt.sprint_exn_var x ; raise exn
    | Llvm2smt.Block_not_found x as exn ->
      Printf.eprintf "%s\n%!" @@ Llvm2smt.sprint_exn_block x ; raise exn

let termite_info =
  let doc = "A termination analyser." in
  Term.info ~doc ~version:Config.version "termite"



let () =
  let open Term in
  let t = pure termite_t $ debug_t $ quiet_t
    $ pagai_t $ pagai_opt_t $ clang_t $ clang_opt_t
    $ algo_t $ file_t
  in
  match eval (ret t,termite_info) with
    | `Ok true -> exit 0
    | `Ok false -> exit 1
    | `Version | `Help -> exit 0
    | `Error _ -> exit (-1)
