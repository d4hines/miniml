type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Int of int
[@@deriving show]
