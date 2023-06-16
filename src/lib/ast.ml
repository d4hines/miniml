type exp =
  | Int of int
  | Bool of bool
  | Var of string
  | Abs of string * exp
  | App of exp * exp
  | Absolute of exp
  | Not of exp
  | Add of exp * exp
  | Sub of exp * exp
  | Div of exp * exp
  | Mul of exp * exp
  | Mod of exp * exp
  | Exp of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Imp of exp * exp
  | Equ of exp * exp
  | GTEqu of exp * exp
  | LTEqu of exp * exp
  | Grt of exp * exp
  | Lst of exp * exp
  | Tup of exp list
  | Proj of exp * exp
  | Ifthenelse of (exp * exp * exp)
