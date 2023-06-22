type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Int of int
[@@deriving show]

let rec find_position el stack acc =
  match stack with
  | hd :: _ when hd = el -> acc
  | _ :: tl -> find_position el tl (acc + 1)
  | [] -> raise Not_found

let find_position el stack = find_position el stack 0

(* TODO: better name *)
let rec to_debruijn expr stack =
  match expr with
  | App (Var "succ", expr) -> Secd.Succ (to_debruijn expr stack)
  | App (Var "pred", expr) -> Secd.Pred (to_debruijn expr stack)
  | App (Var "is_zero", expr) -> Secd.IsZero (to_debruijn expr stack)
  | Var x -> Secd.Var (find_position x stack)
  | Abs (x, expr) ->
      let stack = x :: stack in
      Secd.Abs (to_debruijn expr stack)
  | App (e1, e2) -> Secd.App (to_debruijn e1 stack, to_debruijn e2 stack)
  | Let (x, e1, e2) ->
      Secd.Let (to_debruijn e1 stack, to_debruijn e2 (x :: stack))
  | Int x -> Secd.Int x

let to_debruijn expr = to_debruijn expr []
