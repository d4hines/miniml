type debruijn_expr =
  | DInt of int
  | DVar of int
  | DAbs of debruijn_expr
  | DApp of debruijn_expr * debruijn_expr
  | DLet of debruijn_expr * debruijn_expr
  | DSucc of debruijn_expr
  | DPred of debruijn_expr
  | DIsZero of debruijn_expr
[@@deriving show]

let rec find_position el stack acc =
  match stack with
  | hd :: _ when hd = el -> acc
  | _ :: tl -> find_position el tl (acc + 1)
  | [] -> raise Not_found

let find_position el stack = find_position el stack 0

(* TODO: better name *)
let rec to_debruijn expr stack =
  let open Ast in
  match expr with
  | App (Var "succ", expr) -> DSucc (to_debruijn expr stack)
  | App (Var "pred", expr) -> DPred (to_debruijn expr stack)
  | App (Var "is_zero", expr) -> DIsZero (to_debruijn expr stack)
  | Var x -> DVar (find_position x stack)
  | Abs (x, expr) ->
      let stack = x :: stack in
      DAbs (to_debruijn expr stack)
  | App (e1, e2) -> DApp (to_debruijn e1 stack, to_debruijn e2 stack)
  | Let (x, e1, e2) -> DLet (to_debruijn e1 stack, to_debruijn e2 (x :: stack))
  | Int x -> DInt x

let to_debruijn expr = to_debruijn expr []

let rec compile e =
  let open Secd in
  match e with
  | DInt i -> [ INT i ]
  | DVar x -> [ LOOKUP x ]
  | DAbs i2 -> [ CLOS (compile i2 @ [ RET ]) ]
  | DApp (i1, i2) -> compile i1 @ compile i2 @ [ CALL ]
  | DLet (i1, i2) -> compile i1 @ [ GRAB ] @ compile i2 @ [ ENDLET ]
  | DSucc i -> compile i @ [ SUCC ]
  | DPred i -> compile i @ [ PRED ]
  | DIsZero i -> compile i @ [ ISZERO ]
