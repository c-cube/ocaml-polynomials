
(* This file is free software, part of polynomial. See file "license" for more details. *)

(** {1 Variable} *)

type t

val id : t -> int

val name : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

val make : string -> t
(** @raise Invalid_argument if the name is empty *)

val copy : t -> t

val print : Format.formatter -> t -> unit

val print_full : Format.formatter -> t -> unit
(** Print the name and ID *)

module Set : Set.S with type elt = t
