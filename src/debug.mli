(**
   Debugging and pretty-printing functions
*)


val pp_qarray : Format.formatter -> Q.t array -> unit
val pp_zarray : Format.formatter -> Z.t array -> unit

val pp_qmatrix : Format.formatter -> Q.t array array -> unit
val pp_zmatrix : Format.formatter -> Z.t array array -> unit


(** Nicely format a ranking function on the variables. *)
val pp_coefs : Format.formatter -> Q.t option * Q.t array * 'a Smt.ZZ.term array -> unit


val pp_list :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val pp_array :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a array -> unit
