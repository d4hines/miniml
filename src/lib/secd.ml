(* Adapted from https://github.com/techcentaur/Krivine-Machine/blob/master/SECD.ml *)

type debruijn_expr =
  | Int of int
  | Var of int
  | Abs of debruijn_expr
  | App of debruijn_expr * debruijn_expr
  | Let of debruijn_expr * debruijn_expr
  | Succ of debruijn_expr
  | Pred of debruijn_expr
  | IsZero of debruijn_expr
[@@deriving show]

type opcode =
  | INT of int
  | BOOL of bool
  | LOOKUP of int
  | CLOS of opcode list
  | CALL
  | RET
  | GRAB
  | ENDLET
  | SUCC
  | PRED
  | ISZERO
[@@deriving show]

type table = answer list
and answer = I of int | Vclos of table * control
and stack = answer list
and environment = table
and control = opcode list
and dump = (stack * environment * control) list [@@deriving show]

let initial_env =
  [ (* ("x", I 3);
       ("y", I 5);
       ("z", B true) *) ]

exception InvalidOperation
exception Variable_not_intialized
exception StackError

(* useful functions*)

let lookup x env =
  match List.nth_opt env x with
  | Some ans -> ans
  | None ->
      Format.eprintf "target: %d, stack: %a\n" x pp_environment env;
      raise Variable_not_intialized

let rec compile e =
  match e with
  | Int i -> [ INT i ]
  | Var x -> [ LOOKUP x ]
  | Abs i2 -> [ CLOS (compile i2 @ [ RET ]) ]
  | App (i1, i2) -> compile i1 @ compile i2 @ [ CALL ]
  | Let (i1, i2) -> compile i1 @ [ GRAB ] @ compile i2 @ [ ENDLET ]
  | Succ i -> compile i @ [ SUCC ]
  | Pred i -> compile i @ [ PRED ]
  | IsZero i -> compile i @ [ ISZERO ]

let true_v = Vclos ([], [ CLOS [ LOOKUP 1; INT 0; CALL; RET ]; RET ])
let false_v = Vclos ([], [ CLOS [ LOOKUP 0; INT 0; CALL; RET ]; RET ])

let rec secdmachine state =
  match state with
  | x :: _s, _, [], _ -> x
  | s, e, INT i :: c, d -> secdmachine (I i :: s, e, c, d)
  | s, e, LOOKUP x :: c, d -> secdmachine (lookup x e :: s, e, c, d)
  | s, e, CLOS c' :: c, d -> secdmachine (Vclos (e, c') :: s, e, c, d)
  | x :: Vclos (e', c') :: s, e, CALL :: c, d ->
      secdmachine ([], x :: e', c', (s, e, c) :: d)
  | x :: _s, _e, RET :: _c, (s', e', c') :: d -> secdmachine (x :: s', e', c', d)
  | x :: s, e, GRAB :: c, d -> secdmachine (s, x :: e, c, d)
  | s, _x :: e, ENDLET :: c, d -> secdmachine (s, e, c, d)
  | I i1 :: s, e, SUCC :: c, d -> secdmachine (I (i1 + 1) :: s, e, c, d)
  | I i1 :: s, e, PRED :: c, d ->
      secdmachine (I (if i1 == 0 then 0 else i1 - 1) :: s, e, c, d)
  | I i1 :: s, e, ISZERO :: c, d ->
      secdmachine ((if i1 == 0 then true_v else false_v) :: s, e, c, d)
  | _ -> raise InvalidOperation

let execute oplist = secdmachine ([], initial_env, oplist, [])
