print_endline "-------------------------------------";;
print_endline ("   " ^ Sys.argv.(0));;
print_endline "-------------------------------------";;

open Nts_types;;
open Nts_laure;;
open Debug;;
open Automaton;;

let filename = "nts/multipc.nts";;
let name =
  try
    let _ = Format.printf "Analyzing %s@." filename in 
    print_endline "-------------------------------------";
    Filename.chop_extension (Filename.basename filename)
  with
  | Invalid_argument _ -> Filename.basename filename
;;

(* Get the automaton *)
let automaton, vars = Automaton.read filename "main";;

let tau, states = Ntstosmt.automaton_to_smt vars automaton;;

(* Compute the invariants *)
let invariants = InvariantsDefinitions.get_invariants name;;

let inv = Invariants.formula_to_invariants states vars invariants;;
let smt_inv = Invariants.formula_to_smt states vars invariants;;

(*for i = 0 to Array.length inv - 1 do
  print_string "a_";print_int i;print_string " =";
  for j = 0 to Array.length inv.(i) - 1 do
	print_string " ";
	print_int inv.(i).(j)
  done;
  print_newline ()
done;;*)

let b1 = Buffer.create 1;;
let b2 = Buffer.create 1;;
Smttosmtlib.smt_to_smtlib (Smt.is_int (states.n-1) true) b2 b1;;

(*
Smttosmtlib.smt_to_smtlib smt_inv b2 b1;;
Smttosmtlib.smt_to_smtlib tau b2 b1;;
Smttosmtlib.smt_to_smtlib (Smt.is_ek "v" false "x" (states.n-1) vars.n) b2 b1;;
Smttosmtlib.smt_to_smtlib (Smt.is_diff "u" "v" "w" (states.n*(vars.n+1)-1)) b2 b1;;
*)

print_endline (Buffer.contents b1);;
print_endline (Buffer.contents b2);;

(*let l,s = MonodimensionalMultipc.monodimensionalMultipc vars states inv smt_inv tau true in
print_string "l = ";print_qarray l;print_bool s;;*)

let rho = MultidimensionalMultipc.multidimensionalMultipc vars states inv smt_inv tau true in
print_endline "-----------------";
List.iter Debug.print_qarray rho;;
