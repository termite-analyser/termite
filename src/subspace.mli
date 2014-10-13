(** Operations on subspaces *)

open Smt

(** A subspace. *)
type subspace


(** [empty_subspace n] returns an empty subspace of Q^[n]. *)
val empty_subspace : int -> subspace

(** Returns the size of the subspace. *)
val size : subspace -> int

(** [is_empty subspace] returns [true] if the subspace is empty. *)
val is_empty : subspace -> bool

(** [add_to_subspace subspace v] adds the vector [v] to [subspace].
    If [v] is already in [subspace], doesn't do anything.
*)
val add_to_subspace : subspace -> Q.t array -> unit


(** [is_in subspace v] returns true if and only if [v] is in [subspace]. *)
val is_in : subspace -> Q.t array -> bool

(** [avoid_space subspace u] assert than [u] is not in [subspace]. *)
val avoid_space : subspace -> ZZ.znum ZZ.term array -> [> ZZ.zbool ] ZZ.term


(** {2 Iteration} *)

val iter_by_row :
  ?header:(unit -> unit) -> ?footer:(unit -> unit) ->
  (Q.t -> unit) -> subspace -> unit

val iter_by_column :
  ?header:(unit -> unit) -> ?footer:(unit -> unit) ->
  (Q.t -> unit) -> subspace -> unit

(** {2 printer} *)

val pp : Format.formatter -> subspace -> unit

val to_string : subspace -> string
