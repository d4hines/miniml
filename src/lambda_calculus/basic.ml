type expression =
  | Var of string
  | Abstraction of string * expression
  | Application of expression * expression
[@@deriving show]

let lambda var expr = Abstraction (var, expr)
let apply a b = Application (a, b)
let var x = Var x
let print_expr label expr = Format.printf "%s = %a\n%!" label pp_expression expr
let true_ = lambda "x" (lambda "y" (var "x"))
let false_ = lambda "x" (lambda "y" (var "y"))

let if_ =
  lambda "condition"
    (lambda "consequent"
       (lambda "alternative"
          (apply
             (apply (var "condition") (var "consequent"))
             (var "alternative"))))

let%expect_test "test" =
  print_expr "true" true_;
  print_expr "false" false_;
  [%expect
    {|
    true = (Basic.Abstraction ("x", (Basic.Abstraction ("y", (Basic.Var "x")))))
    false = (Basic.Abstraction ("x", (Basic.Abstraction ("y", (Basic.Var "y"))))) |}]

let rec substitute var body expr =
  match body with
  | Var x when x = var -> expr
  | Var x -> Var x
  | Abstraction (var', body) -> Abstraction (var', substitute var body expr)
  | Application (a, b) ->
    Application (substitute var a expr, substitute var b expr)

let%expect_test "substitution" =
  let expr = lambda "x" (lambda "y" (lambda "z" (apply (var "x") (var "x")))) in
  print_expr "expr" expr;
  let substituted = substitute "x" expr (var "foo") in
  print_expr "substituted" substituted;
  [%expect
    {|
      expr = (Basic.Abstraction ("x",
                (Basic.Abstraction ("y",
                   (Basic.Abstraction ("z",
                      (Basic.Application ((Basic.Var "x"), (Basic.Var "x")))))
                   ))
                ))
      substituted = (Basic.Abstraction ("x",
                       (Basic.Abstraction ("y",
                          (Basic.Abstraction ("z",
                             (Basic.Application ((Basic.Var "foo"),
                                (Basic.Var "foo")))
                             ))
                          ))
                       )) |}]

let rec eval expr =
  match expr with
  | Application (a, b) -> (
    (* Format.printf "Application: %a %a\n" pp_expression a pp_expression b; *)
    let a, b = (eval a, eval b) in
    match a with
    | Abstraction (var, body) -> eval @@ substitute var body b
    | _ -> Application (a, b))
  | Var var -> Var var
  | Abstraction (var, expr) -> Abstraction (var, eval expr)

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
    ex1 = (Basic.Abstraction ("x", (Basic.Abstraction ("y", (Basic.Var "z")))))
    ex2 = (Basic.Var "y")
    ex2 = (Basic.Var "z")
    ex3 = (Basic.Abstraction ("x", (Basic.Var "x")))
    ex4 = (Basic.Abstraction ("y", (Basic.Var "a")))
    ex5 = (Basic.Var "a")
    expr6 = (Basic.Abstraction ("condition",
               (Basic.Abstraction ("consequent",
                  (Basic.Abstraction ("alternative",
                     (Basic.Application (
                        (Basic.Application ((Basic.Var "condition"),
                           (Basic.Var "consequent"))),
                        (Basic.Var "alternative")))
                     ))
                  ))
               ))
    expr7 = (Basic.Var "foo") |}]

(*
(\x \x x) a b => b
*)
