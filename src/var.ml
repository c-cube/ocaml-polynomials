
(* This file is free software, part of polynomial. See file "license" for more details. *)

(** {1 Variable} *)

type t = {
  id: int;
  name: string;
}

let id t = t.id
let name t = t.name

let equal a b = a.id = b.id

let compare a b = Pervasives.compare a.id b.id

let make =
  let n = ref 0 in
  fun name ->
    if name="" then invalid_arg "Var.make";
    let v = {name; id= !n ; } in
    incr n;
    v

let copy v = make v.name

let print out v = Format.pp_print_string out v.name

let print_full out v = Format.fprintf out "%s/%d" v.name v.id

module Set = Set.Make(struct
  type t_ = t
  type t = t_
  let compare = compare
end)
