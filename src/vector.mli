(**
   Simple vector operations (using Q.t arrays)
*)

open Smt

(**
   [isNull v] returns true if v is the null vector
*)
val isNull : Q.t array -> bool

(**
   [hasNoNullExcept v r] returns [true] if v has no null coordinate,
   except the ones in [r], where [r] is a list of integers
*)
val hasNoNullExcept : Q.t array -> int list -> bool

(**
   [sub v1 v2] returns [v1] - [v2]
*)
val sub : Q.t array -> Q.t array -> Q.t array

(**
   [scalar v1 v2 n] returns the scalar product of [v1] and [v2].
*)
val scalar : Q.t array -> Q.t array -> Q.t



(** Operations on vector of terms. *)
module T : sig

  val create :
    ('a, 'b) ZZ.typ -> string -> int -> ('a, 'b) ZZ.symbol array

  val term : ('a, 'b) ZZ.symbol array -> 'b ZZ.term array

  val symbol : ('a, 'b) ZZ.typ -> 'b ZZ.term array -> ('a, 'b) ZZ.symbol array

  val get_value : model:Z3.Model.model -> ('a, 'b) ZZ.symbol array -> 'a array

  (** [scalar u l] returns the expression "u.l" *)
  val scalar :
    ([< ZZ.znum ] as 'a) ZZ.term array -> 'a ZZ.term array -> 'a ZZ.term

  (** Same as {!scalar} with [l] an array of rationals. *)
  val scalar_q :
    ([< ZZ.znum > `Real ] as 'a) ZZ.term array -> Q.t array -> 'a ZZ.term

  (** Same as {!scalar} with [l] an array of rationals. *)
  val scalar_z :
    ([< ZZ.znum > `Int ] as 'a) ZZ.term array -> Z.t array -> 'a ZZ.term

  val sum : ([< ZZ.znum ] as 'a) ZZ.term array -> 'a ZZ.term

  (** [sub v1 v2] returns [v1] - [v2] *)
  val sub : ([< ZZ.znum ] as 'a) ZZ.term array -> 'a ZZ.term array -> 'a ZZ.term array

  (** [eq v1 v2] returns [v1] = [v2] *)
  val eq : 'a ZZ.term array -> 'a ZZ.term array -> [> ZZ.zbool ] ZZ.term

  (** [eq v1 v2] returns [v1] <> [v2] *)
  val neq : 'a ZZ.term array -> 'a ZZ.term array -> [> ZZ.zbool ] ZZ.term

  (**
     [unsatisfied_by_ranking l] returns the expression "u.l <= 0"
  *)
  val unsatisfied_by_ranking :
    [< ZZ.znum > `Real ] ZZ.term array -> Q.t array -> [> ZZ.zbool ] ZZ.term

  (** [is_null u] returns u = 0 *)
  val is_null : expression array -> relation

  (** [is_ek v k x states vars] returns the expression v = e_k(x) *)
  val is_ek :
    ('a, ZZ.znum) ZZ.symbol array -> expression -> ('a, ZZ.znum) ZZ.symbol array -> int -> int -> relation

  (** [is_diff u v1 v2] returns the expression ∀ n, u[n] = v1[n] - v2[n] *)
  val is_diff :
    ([< ZZ.znum ] as 'a) ZZ.term array ->
    'a ZZ.term array ->
    'a ZZ.term array -> ZZ.zbool ZZ.term

end
