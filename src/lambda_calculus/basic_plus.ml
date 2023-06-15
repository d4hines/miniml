type expression =
  | Var of string
  | Abstraction of string * expression
  | Application of expression * expression
[@@deriving show]

module String_map = Map.Make (String)

type frame = expression String_map.t

let rec eval frame expr =
  match expr with
  | Var var -> String_map.find_opt var frame |> Option.value ~default:expr
  | Abstraction (param, body) -> Abstraction (param, eval frame body)
  | Application (a, b) -> (
      let a, b = (eval frame a, eval frame b) in
      match a with
      | Abstraction (param, body) -> eval (String_map.add param b frame) body
      | _ -> Application (a, b))

let eval expr = eval String_map.empty expr
let lambda var expr = Abstraction (var, expr)
let apply a b = Application (a, b)
let var x = Var x
let true_ = lambda "x" (lambda "y" (var "x"))
let false_ = lambda "x" (lambda "y" (var "y"))

let if_ =
  lambda "condition"
    (lambda "consequent"
       (lambda "alternative"
          (apply
             (apply (var "condition") (var "consequent"))
             (var "alternative"))))

let print_expr label expr = Format.printf "%s = %a\n%!" label pp_expression expr

let%expect_test "eval" =
  print_expr "ex1" @@ eval (lambda "x" (lambda "y" (var "z")));
  print_expr "ex2" @@ eval (apply (lambda "x" (var "x")) (var "y"));
  print_expr "ex2"
  @@ eval
       (apply (lambda "x" (var "x")) (apply (lambda "y" (var "y")) (var "z")));

  print_expr "ex3"
  @@ eval
       (apply
          (lambda "z"
             (apply
                (lambda "x" (var "x"))
                (apply (lambda "y" (var "y")) (var "z"))))
          (lambda "x" (var "x")));
  print_expr "ex4" (eval (apply (lambda "x" (lambda "y" (var "x"))) (var "a")));
  print_expr "ex5" (eval (apply (apply true_ (var "a")) (var "b")));
  print_expr "expr6" (eval if_);
  print_expr "expr7"
    (eval (apply (apply (apply if_ true_) (var "foo")) (var "bar")));
  [%expect
    {|
    ex1 = (Basic_plus.Abstraction ("x",
             (Basic_plus.Abstraction ("y", (Basic_plus.Var "z")))))
    ex2 = (Basic_plus.Var "y")
    ex2 = (Basic_plus.Var "z")
    ex3 = (Basic_plus.Abstraction ("x", (Basic_plus.Var "x")))
    ex4 = (Basic_plus.Abstraction ("y", (Basic_plus.Var "a")))
    ex5 = (Basic_plus.Var "a")
    expr6 = (Basic_plus.Abstraction ("condition",
               (Basic_plus.Abstraction ("consequent",
                  (Basic_plus.Abstraction ("alternative",
                     (Basic_plus.Application (
                        (Basic_plus.Application ((Basic_plus.Var "condition"),
                           (Basic_plus.Var "consequent"))),
                        (Basic_plus.Var "alternative")))
                     ))
                  ))
               ))
    expr7 = (Basic_plus.Var "foo") |}]
