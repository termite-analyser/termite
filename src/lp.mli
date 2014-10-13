(**
   Module used for LP solving.
*)

(**
   [lp c cons_I] solves the LP(c, cons_I) instance and returns [lambda] and [delta].
*)
val lp_one_control_point : ?verbose:bool -> Q.t array list -> Z.t array array -> (Q.t array * Q.t array)
