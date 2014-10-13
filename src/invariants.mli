(** Extract invariants from an llvm files annotated by pagai. *)

open Llvm

(** Basic relations than can appear in an invariant. *)
type rel = Ge | Gt | Eq

(** Transform ">=", ">" or "==" into the appropriate {!rel}. *)
val rel_of_string : string -> rel

(** The face of a polyhedron. *)
type face = {
  rel : rel ;
  constant : Z.t ;
  coefs : Z.t array ;
}

(** An invariant at a specified control point, on a specified set of variable. *)
type invariant = {
  control_point : llbasicblock ;
  variables : llvalue array ;
  polyhedron : face array ;
  (** A polyhedron represented as a list of faces. *)
}

(** {2 Lp} *)

(** [to_matrix i] returns the constraints of the invariant i as a matrix. *)
val to_matrix : invariant -> Z.t array array

(** Same as {!to_matrix} but with a list of invariants. Also returns the variables and the constants in the correct order. *)
val group_to_matrix : invariant list -> llvalue array * Z.t array * Z.t array array

(** If invs is a group of invariants [get_var_to_cp invs] returns a function that associate a position in the variable array obtained by {!group_to_matrix} to the index of the control_point in [invs]. *)
val get_var_to_cp : invariant list -> (int -> int option)

(** {2 Smt} *)
open Smt.ZZ

(** Transform a {!rel} into the appropriate Z3 operator. *)
val rel_to_smt : rel -> ([< znum ] as 'a) term -> 'a term -> [> zbool ] term

(** Encode to smt. Need a dictionary from llvalues to smt variables. *)
val to_smt : (llvalue -> [< znum ] term) -> invariant -> [> zbool ] term


(** {2 Pagai} *)

(** Extract the invariants from an llvm function. *)
val from_llfun : llvalue -> invariant list

(** {3 Internal functions} *)

(** Collect all the variables than can be in an invariant.

    In order to do that :
    - We go through all the instructions to find the !pagai.var annotations.
    - We add the function parameters.
*)
val get_vars : llvalue -> (llvalue, llvalue) Hashtbl.t

(** Collect all the pagai.invariant metadatas. *)
val get_invariant_metadatas :  llvalue -> (llbasicblock * llvalue list) list

(** Extract invariants from a metada pagai.invariant.
    Returns the polyhedron and the list of variables involved.
*)
val extract_invariants : (llvalue, 'a) Hashtbl.t -> llvalue -> 'a array * face list


(** Compute control points according to pagai, from association list from basicbloc to invariant metadatas.

    The control points, according to pagai, are all the places where there are some invariants,
    except the first node (no predecessor) and the return node (no successor).
*)
val get_pagai_control_points : (llbasicblock * 'a list) list -> llbasicblock list
