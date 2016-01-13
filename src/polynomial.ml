
(* This file is free software, part of polynomial. See file "license" for more details. *)

module IMap = Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end)

type t =
  | Const of Z.t
  | Poly of Var.t * t IMap.t

let is_zero = function
  | Const n -> Z.sign n = 0
  | Poly (_, m) ->
      assert (not (IMap.is_empty m));
      false

let zero = Const Z.zero

let const c = Const c

let make_ v m =
  if IMap.is_empty m then zero
  else Poly (v, m)

let make var coeffs =
  IMap.iter
    (fun _ c -> match c with
      | Const _ -> ()
      | Poly (var', m) ->
          assert (not (IMap.is_empty m));
          if Var.compare var var' <= 0 then invalid_arg "Polynomial.make")
    coeffs;
  make_ var coeffs

let make_const v coeffs =
  let coeffs = IMap.map const coeffs in
  if not (IMap.for_all (fun _ c -> not (is_zero c)) coeffs)
    then invalid_arg "Polynomial.make_const";
  make_ v coeffs

let get_0 m =
  try IMap.find 0 m
  with Not_found -> zero

(* if [c = 0], then remove it, else add it *)
let add_map i c m =
  if is_zero c
  then IMap.remove i m
  else IMap.add i c m

let rec add_const c p = match p with
  | Const c' -> const Z.(c + c')
  | Poly (v, m) ->
      let p' = get_0 m in
      let c = add_const c p' in
      make_ v (add_map 0 c m)

let rec add p1 p2 = match p1, p2 with
  | Const c1, Const c2 -> const Z.( c1 + c2 )
  | Const c, (Poly _ as p)
  | (Poly _ as p), (Const c) -> add_const c p
  | Poly (v1, m1), Poly (v2, m2) ->
      match Var.compare v1 v2 with
      | 0 ->
          (* add pairwise *)
          let m =
            IMap.merge
              (fun _ c1 c2 -> match c1, c2 with
                | None, None -> assert false
                | Some c, None
                | None, Some c -> Some c
                | Some c1, Some c2 ->
                    let c = add c1 c2 in
                    if is_zero c then None else Some c)
              m1 m2
          in
          make_ v1 m
      | n when n<0 ->
          (* add p1 to p2.coeffs.0 *)
          let c = add p1 (get_0 m2) in
          make_ v2 (add_map 0 c m2)
      | _ ->
          (* add p2 to p1.coeffs.0 *)
          let c = add p2 (get_0 m1) in
          make_ v1 (add_map 0 c m1)

let add_monome l c p = match l with
  | [_, 0]
  | [] -> add_const c p
  | _ ->
      let l = List.sort (fun (v1,_)(v2,_) -> Var.compare v1 v2) l in
      let p' =
        List.fold_left
          (fun p' (v,coeff) -> make_ v (IMap.singleton coeff p'))
          (const c)
          l
      in
      add p' p

let of_list l =
  List.fold_left
    (fun p (l,c) -> add_monome l c p)
    zero l

let rec mult_const c p = match p with
  | Const c' -> const Z.(c * c')
  | Poly (v, m) -> make_ v (IMap.map (mult_const c) m)

let rec mult p1 p2 = match p1, p2 with
  | Const c1, Const c2 -> const Z.(c1 * c2)
  | Const c, Poly (v, m)
  | Poly (v, m), Const c ->
      let c' = get_0 m in
      make_ v (add_map 0 (mult_const c c') m)
  | Poly (v1, m1), Poly (v2, m2) ->
      match Var.compare v1 v2 with
      | 0 ->
          (* product of m1 and m2 *)
          let m =
            IMap.fold
              (fun pow1 c1 m ->
                IMap.fold
                  (fun pow2 c2 m ->
                    let c = mult c1 c2 in
                    add_map (pow1+pow2) c m)
                  m2 m)
              m1 IMap.empty
          in
          make_ v1 m
      | n when n<0 ->
          make_ v2 (IMap.map (mult p1) m2)
      | _ ->
          make_ v1 (IMap.map (mult p2) m1)

let print out p =
  let pp_vars out l =
    List.iter
      (function
        | 0, _ -> ()
        | 1, v -> Format.fprintf out " × %a" Var.print v
        | pow, v -> Format.fprintf out " × %a^%d" Var.print v pow)
      l
  in
  let rec pp vars out = function
    | Const z ->
        Format.fprintf out "@[<h>%a%a@]" Z.pp_print z pp_vars vars
    | Poly (v, coeffs) -> pp_coeffs v vars out coeffs
  and pp_coeffs v vars out m =
    let first = ref true in
    IMap.iter
      (fun pow c ->
        if !first then first := false else Format.fprintf out " +@ ";
        pp ((pow,v) :: vars) out c)
        m
  in
  Format.fprintf out "@[<hv>";
  pp [] out p;
  Format.fprintf out "@]";
  ()
