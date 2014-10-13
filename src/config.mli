(**
   Configuration
*)

val version : string

(**
   If [debug] is false, termite only displays the resulting ranking function,
   else it details the steps of the computation
*)
val debug : bool ref

(**
   Path of the pip executable
*)
val pip : string ref
