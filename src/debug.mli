(**
   Debugging and pretty-printing functions
*)


val pp_qarray : Format.formatter -> Q.t array -> unit
val pp_zarray : Format.formatter -> Z.t array -> unit

val pp_qmatrix : Format.formatter -> Q.t array array -> unit
val pp_zmatrix : Format.formatter -> Z.t array array -> unit


val pp_coefs : Format.formatter -> Q.t array * 'a Smt.ZZ.term array -> unit

(** Nicely format a ranking function on the variables. *)
val pp_ranking_fun : Format.formatter -> Q.t * Q.t array * 'a Smt.ZZ.term array -> unit


val pp_list :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val pp_array :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a array -> unit


(** Utils *)

(** Result returned by an algorithm *)
type 'a result = {
  result : 'a ;
  loops : int ;
  lp_max_size : int * int ;
  control_points : int ;
}

val pp_result :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a result -> unit
