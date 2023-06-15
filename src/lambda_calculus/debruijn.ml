type nat = int [@@deriving show]

type expression =
  | Var of nat
  | Abstraction of expression
  | Application of expression * expression

type t = expression

let rec string_of_expression = function
  | Var n -> string_of_int n
  | Abstraction body -> Format.sprintf "(λ %s)" (string_of_expression body)
  | Application (a, b) ->
      Format.sprintf "(%s) %s" (string_of_expression a) (string_of_expression b)

let pp fmt expr = Format.fprintf fmt "%s" (string_of_expression expr)

let rec substitute var body substitution =
  match body with
  | Var n when n = var -> substitution
  | Var n -> Var n
  | Application (a, b) ->
      Application (substitute var a substitution, substitute var b substitution)
  | Abstraction body -> Abstraction (substitute (var + 1) body substitution)

let print_expr label expr = Format.printf "%s => %a\n%!" label pp expr

let rec eval body =
  (* print_expr "intermediate" body; *)
  match body with
  | Var n -> Var n
  | Abstraction body -> Abstraction (eval body)
  | Application (a, b) -> (
      let a, b = (eval a, eval b) in
      match a with
      | Abstraction body -> eval @@ substitute 1 body b
      | a -> Application (a, b))

let lambda expr = Abstraction expr
let apply a b = Application (a, b)
let var x = Var x
let true_ = lambda (lambda (var 2))
let false_ = lambda (lambda (var 1))
let if_ = lambda (lambda (lambda (apply (apply (var 3) (var 2)) (var 1))))

let%expect_test "eval" =
  print_expr "(\\x.x) y = y" @@ eval (apply (lambda (var 1)) (var 2));
  print_expr "true 99 42 = 99" @@ eval (apply (apply true_ (var 99)) (var 42));
  print_expr "eval (if true)" @@ eval (apply if_ true_);
  print_expr "if true then 99 else 42 = 99"
  @@ eval (apply (apply (apply if_ true_) (var 99)) (var 42));
  [%expect
    {|
    (\x.x) y = y => 2
    true 99 42 = 99 => 99
    eval (if true) => (λ (λ 2))
    if true then 99 else 42 = 99 => 99 |}]
