[@@@warning "-27"]

(* Adapted from https://github.com/techcentaur/Krivine-Machine/blob/master/SECD.ml *)

(* Expressions *)

open Ast.Phase0

(* Opcode list generated by compiler *)

type opcode =
  | INT of int
  | BOOL of bool
  | LOOKUP of string
  | CLOS of string * opcode list
  | CALL
  | RET
  | SUCC
  | PRED
  | ISZERO
[@@deriving show]

(* Interdependent types *)
type table = (string * answer) list
and answer = I of int | Vclos of table * string * control | T of answer list
and stack = answer list
and environment = table
and control = opcode list
and dump = (stack * environment * control) list [@@deriving show]

let initial_env =
  [ (* ("x", I 3);
       ("y", I 5);
       ("z", B true) *) ]

(* exceptions *)
exception InvalidOperation
exception Variable_not_intialized
exception StackError

(* useful functions*)

let rec lookup x env =
  match env with
  | [] -> raise Variable_not_intialized
  | (str, ans) :: env' -> if str = x then ans else lookup x env'

(* compile function *)
let rec compile e =
  match e with
  | Int i -> [ INT i ]
  | Var x -> [ LOOKUP x ]
  | Abs (x, i2) -> [ CLOS (x, compile i2 @ [ RET ]) ]
  | App (i1, i2) -> compile i1 @ compile i2 @ [ CALL ]
  | Succ i -> compile i @ [ SUCC ]
  | Pred i -> compile i @ [ PRED ]
  | IsZero i -> compile i @ [ ISZERO ]

let true_v =
  Vclos ([], "x", [ CLOS ("y", [ LOOKUP "x"; INT 0; CALL; RET ]); RET ])

let false_v =
  Vclos ([], "x", [ CLOS ("y", [ LOOKUP "y"; INT 0; CALL; RET ]); RET ])

(* secdmachine execution function *)
let rec secdmachine = function
  | x :: s, _, [], _ -> x
  | s, e, INT i :: c, d -> secdmachine (I i :: s, e, c, d)
  | s, e, LOOKUP x :: c, d -> secdmachine (lookup x e :: s, e, c, d)
  | s, e, CLOS (x, c') :: c, d -> secdmachine (Vclos (e, x, c') :: s, e, c, d)
  | x :: Vclos (e', x', c') :: s, e, CALL :: c, d ->
      secdmachine ([], (x', x) :: e', c', (s, e, c) :: d)
  | x :: s, e, RET :: c, (s', e', c') :: d -> secdmachine (x :: s', e', c', d)
  | I i1 :: s, e, SUCC :: c, d -> secdmachine (I (i1 + 1) :: s, e, c, d)
  | I i1 :: s, e, PRED :: c, d ->
      secdmachine (I (if i1 == 0 then 0 else i1 - 1) :: s, e, c, d)
  | I i1 :: s, e, ISZERO :: c, d ->
      secdmachine ((if i1 == 0 then true_v else false_v) :: s, e, c, d)
  | _ -> raise InvalidOperation

(* execute call *)
let execute oplist = secdmachine ([], initial_env, oplist, [])
