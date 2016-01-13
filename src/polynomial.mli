
(* This file is free software, part of polynomial. See file "license" for more details. *)

(* TODO: functorize over the coefficients *)

module IMap : Map.S with type key = int

(** A polynomial.

    Invariants:
    - in [Poly (v, m)], [v] is always
      bigger (w.r.t {!Var.compare}) than any variable in its
      coefficients.
    - in [Poly (v, m)], [m] is always non empty and all the coefficients are non-zero
*)
type t = private
  | Const of Z.t
  | Poly of Var.t * t IMap.t (* power -> coefficient *)

val is_zero : t -> bool

val zero : t

val const : Z.t -> t

val make : Var.t -> t IMap.t -> t
(** Build a polynomial
    @raise Invalid_argument if [v] is not larger than some variable in the coefficients *)

val make_const : Var.t -> Z.t IMap.t -> t
(** @raise Invalid_argument if some coefficient is 0 *)

val add_const : Z.t -> t -> t

val add_monome : (Var.t * int) list -> Z.t -> t -> t
(** [add_monome l z p] adds the [z × Π_i (x_i ^ p_i)] to [p] *)

val of_list : ((Var.t * int) list * Z.t) list -> t
(** Build a polynomial from a list of monomes *)

val mult : t -> t -> t

val add : t -> t -> t

val print : Format.formatter -> t -> unit

