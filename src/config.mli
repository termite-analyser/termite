(**
   Configuration
*)

val version : string

(**
   If [debug] is false, termite only displays the resulting ranking function,
   else it details the steps of the computation
*)
val debug : bool ref


(** If [compact] is true, termites will output more compact information. *)
val compact : bool ref
