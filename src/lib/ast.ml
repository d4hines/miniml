module Phase0 = struct
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
end

module Phase1 = struct
  (* type exp =
       | PlusExp of exp * exp
       | SubExp of exp * exp
       | MulExp of exp * exp
       | DivExp of exp * exp
       | Variable of string
       | Number of int
       | LTExp of exp * exp
       | GTExp of exp * exp
       | AndExp of exp * exp
       | OrExp of exp * exp
       | Bool of bool
       | Fun of string * exp
       | App of exp * exp
       | Let of string * exp * exp
       | IfThenElse of exp * exp * exp
     [@@deriving show] *)

  (*
      let x = (a b) in
      let y = fun x -> fun y -> x in
      if false then
        ((y (x + x)) 0)
      else
        7
   *)
  (*
      IDENT = [a-z]+
      LET := let IDENT = EXPR in EXPR
      INT := [0-9]+
      VAR := IDENT
      ATOM := VAR | INT
      LAMBDA := fun x -> EXPR
      APPLY_ABLE :=
      APP := ((LAMBDA) ()) |
      EXPR = VAR | LET | ATOM
  *)
  type exp =
    | Int of int
    | Bool of bool
    | Var of string
    | Abs of string * exp
    | App of exp * exp
    | Add of exp * exp
    | Sub of exp * exp
    | Equ of exp * exp
    | Ifthenelse of (exp * exp * exp)

  (*
        
       int := [0-9]+
       bool := #t | #f
       var := [a-z]+
       op := + | - | =
       atom := int | bool | var | op
       if := (if expr expr expr)
       fun := (fun var expr)
       let := (let )
       app := (expr expr)
       expr :=  atom | if | fun | let | app
  *)
end

module Phase2 = struct
  type expr =
    | Var of string
    | Abs of string * expr
    | App of expr * expr
    | Let of string * expr * expr
    | Int of int
  [@@deriving show]
end
