(** Operations on subspaces *)

(** A subspace of Q^n is represented by a matrix in row-echelon form.

    The matrix is lower triangular of size n*n.

    A column is a couple of the first non null index (or [Null] if the column is null)
    and the actual column.
*)

(** The Index of the first non null value, or Null if the column is null. *)
type index = Index of int | Null

type column = { first_non_null_index : index ; column : Q.t array }

type subspace = column array


(** [empty_column n] is a null column of size [n]. *)
let empty_column n = { first_non_null_index = Null ; column = Array.make n Q.zero }

(** [empty_subspace n] returns an empty subspace of Q^[n]. *)
let empty_subspace n =
  Array.init n (fun _ -> empty_column n)

(** Return [Null] if the vector is null and [Index i] the index
    of the first non null element of the vector otherwise.
*)
let first_non_null_index v =
  let l = Array.length v in
  let rec aux i =
    if i >= l then Null
    else if Q.equal v.(i) Q.zero then aux (i+1)
    else Index i
  in aux 0

(** [make_column v] creates a column from the vector [v]. *)
let make_column v =
  let i = first_non_null_index v in
  { first_non_null_index = i ; column = v }

(** {2 Misc} *)

let size = Array.length

let iter_by_row ?(header=ignore) ?(footer=ignore) f subspace =
  let n = size subspace in
  for i = 0 to n - 1 do
    header () ;
    for j = 0 to n - 1 do
      f (subspace.(j).column.(i))
    done ;
    footer () ;
  done

let iter_by_column ?(header=ignore) ?(footer=ignore) f subspace =
  Array.iter (fun col ->
    header () ;
    Array.iter f col.column ;
    footer () ;
  )
    subspace

let is_null_column v = match v.first_non_null_index with Null -> true | _ -> false

let is_empty subspace =
  Array.fold_left (fun b col -> is_null_column col && b) true subspace


(** {2 Base related functions} *)

(** (<) operator, adapted for indexes, such that [Some _ <& Null] and [Null <& Null].

    It basically compares the number of zeros.
    ∀ [i, j = 0 to (n-1)], [i < j] => [a.(i).first_non_null_index <& a.(j).first_non_null_index].
*)
let (<&) idx1 idx2 = match idx1, idx2 with
  | _, Null -> true
  | Null, Index _ -> false
  | Index i, Index j -> i < j



(** Check that the subspace matrix is indeed in row echelon form. *)
let is_row_echelon subspace =
  let n = size subspace in
  let rec aux i previous_index =
    if i >= 0 then true
    else begin
      let col = subspace.(i) in
      let idx = first_non_null_index col.column in
      idx = col.first_non_null_index &&
      previous_index <& idx  &&  aux (i+1) idx
    end
  in
  n = 0 || (Array.length subspace.(0).column = n && aux 0 (Index 0))



(** [pivot k t1 t2] ≅ [t1 <- t1 - t1.(k) / t2.(k) * t2]
    pivot [t1] on [t2] on the [k]-th coordinates.
*)
let pivot k t1 t2 =
  if Array.length t1 <> Array.length t2 then
    raise (Invalid_argument "pivot : Vector do not have the same length.") ;
  let t1_k = t1.(k) in
  Array.iteri (fun i t2_i -> t1.(i) <- Q.(t1.(i) - (t2_i * t1_k) / t2.(k))) t2

(** [transvection subspace v] computes
    [v <- v - v.(idx i) / subspace.(i).(idx i) * subspace.(i)]
    with [idx i = subspace.(i).first_non_null_index], for all i.

    It will produce a column with everything null above the "diagonal"
    and that can be inserted in the row echelon matrix.
*)
let transvection subspace v =
  let v = Array.copy v in
  subspace |> Array.iter
    (fun { first_non_null_index ; column } ->
       match first_non_null_index with
         | Null -> ()
         | Index k -> pivot k v column
    ) ;
  make_column v



(** [insert_at a x i] insert an element [x] in [a] at the index [i] by shifting
    the other element by one to the right. Throw away the last element.
*)
let insert_at a x i =
  let n = Array.length a in
  for k = n - 1 downto i + 1 do
    a.(k) <- a.(k - 1)
  done ;
  a.(i) <- x

(** Insert a column at the right place in the row echelon matrix. *)
let insert_column subspace col =
  let n = size subspace in
  if not (is_null_column subspace.(n - 1)) then
    raise (Invalid_argument "insert_column: The last column is not null") ;

  let idx = col.first_non_null_index in
  let rec aux i =
    (* if the column couldn't be inserted anywhere, something has gone wrong. *)
    if i >= n then raise (Invalid_argument "insert_column: The column can't be inserted.") ;

    (* Let's consider the column i of the matrix. *)
    (* If [col] is smaller (has less leading zeros) than the current column, we insert here! *)
    if idx <& subspace.(i).first_non_null_index then insert_at subspace col i
    (* If not, then just carry on. *)
    else aux (i+1)
  in
  aux 0


(** [add_to_subspace subspace v] adds the vector [v] to [subspace] and
    take care of maintaining the row echelon form.

    If [v] is already in [subspace], doesn't do anything.
*)
let add_to_subspace subspace v =
  (* First, let's do the transvection. *)
  let col = transvection subspace v in

  (* If the column is null, then don't add it to the subspace. *)
  if col.first_non_null_index = Null then ()
  (* Otherwise, we can add it safely. *)
  else insert_column subspace col


(** [is_in subspace v] returns true if and only if [v] is in [subspace]. *)
let is_in subspace v =
  let col = transvection subspace v in

  col.first_non_null_index = Null



(** {2 Avoid subspace} *)

open Smt.ZZ

(** [update_add t1 k t2] ≡ ∀i [t1.(i) <- t1.(i) + k * t2.(i)]. *)
let update_combi t1 k t2 =
  Array.iteri (fun i t2_i -> t1.(i) <- T.(t1.(i) + k * rat t2_i)) t2

(** [unit n i] is eᵢ of size [n]. *)
let unit n i =
  let t = Array.make n Q.zero in
  t.(i) <- Q.one ;
  t


(** Consider the family of vector B described by [subspace]. We complete this to a base (B,B').
    A vector [u] is not in [subspace] if and only if we can express u in the base B
    and the coefficients on the B' part of the base are not all null.

    More formally :
    u ∉ Span([subspace]) if and only if
    ∃(βᵢ)vᵢ∈B ∃(γᵢ)vᵢ∈B' u = Σi∈B βᵢvᵢ + Σi∈B' γᵢvᵢ  and  ∃i γᵢ ≠ 0
*)
let avoid_space subspace u =
  let n = Array.length u in
  if size subspace <> n then
    raise (Invalid_argument "avoid_space: the sizes of the subspace and the vector don't match.") ;

  (* This is the βᵢ and γᵢ. *)
  let bvar = Vector.T.create Real "base" n in

  (* This is the base decomposition [u] should be equal to. *)
  let lin_combi_u = Array.make n (T.rat Q.zero) in


  (** This function will go though the n dimensions of Q^n and either pick the vector
      from subspace or a canonical vector. It updates lin_combi_u for each dimension.
  *)
  (* if [dim] is the number of considered dimension, [k = n - dim].
     [i] is the actual index we are looking at in the [subspace] matrix.

     We have always [0 ≤ i ≤ k ≤ n].

     [gamma] = ∨i γᵢ ≠ 0.
  *)
  let rec aux k i gamma =
    if k >= n then T.or_ gamma
    else begin
      let col = subspace.(i) in
      match col.first_non_null_index with
        (* This dimension have one of the vector of the subspace family (called [col]).
           We add [βₖ * col] to [lin_combi_u]. *)
        | Index idx when idx = k ->
            update_combi lin_combi_u (T.symbol bvar.(k)) col.column ;
            aux (k+1) (i+1) gamma

        (* There is no vector of this dimension. This is in the completed base.
           We add [γₖ * eₖ] to [lin_combi_u]. *)
        | Null | Index _ ->
            update_combi lin_combi_u (T.symbol bvar.(k)) (unit n k) ;
            aux (k+1) i (T.(!(bvar.(k)) <> int 0) :: gamma)
    end
  in

  let gamma = aux 0 0 [] in
  T.and_ [ Vector.T.eq u lin_combi_u ; gamma ]

(** {2 Printers} *)

let pp fmt subspace =
  let header () = Format.pp_print_string fmt "|" in
  let footer () = Format.fprintf fmt " |@," in
  Format.pp_open_vbox fmt 0 ;
  iter_by_row ~header ~footer
    (fun q -> Format.fprintf fmt " %a" Q.pp_print q)
    subspace ;
  Format.pp_close_box fmt ()

let to_string subspace = Format.asprintf "%a" pp subspace
