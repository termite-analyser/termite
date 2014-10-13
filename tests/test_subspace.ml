print_endline "-------------------------------------";;
print_endline ("   " ^ Sys.argv.(0));;
print_endline "-------------------------------------";;

open Debug;;

Random.self_init ();;

(**
   [random_subspace n m bn bd] creates a random subspace of dimension
   m of Q^n, with the numerator of the coefficients of the basis'
   vectors lower than [bn] and the denomirator lower than [bd].
*)
let random_subspace n m bn bd =
  let s = Subspace.empty_subspace n in
  print_newline ();
  print_endline "Vectors of the basis :";
  for i = 0 to m - 1 do
	let v = Array.make n Q.zero in
	for j = 0 to n - 1 do
	  v.(j) <- Q.of_ints (Random.int bn) (Random.int bd + 1)
	done;
	print_string "v_";print_int i;print_string " = ";print_qarray v;
	if Subspace.is_in s v then
	  print_endline "in"
	else
	  Subspace.add_to_subspace s v
  done;
  s
;;

let s = random_subspace 7 10 2 1 in
print_newline ();
print_qmatrix s;
print_newline ();

let b = Buffer.create 1024 in
Smttosmtlib.smt_to_smtlib (Smt.avoid_space s) b (Buffer.create 1024);;
