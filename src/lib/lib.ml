let message = "hello world"

let%expect_test "dummy test" =
  print_endline message;
  [%expect {|hello world |}]

open Ast.Phase2

let show_ast label x =
  let lexbuf = Lexing.from_string x in
  let result = Parser.expr Lexer.token lexbuf in
  Format.printf "%s: %a\n%!" label pp_expr result

let%expect_test "parsing tests" =
  show_ast "lets" {|
    let x = fun x -> 1 in
    let y = fun y -> 2 in
    3|};
  show_ast "apply 1" {|(a b)|};
  show_ast "apply 2" {|((a b) c)|};
  [%expect
    {|
      lets: (Ast.Phase2.Let ("x", (Ast.Phase2.Abs ("x", (Ast.Phase2.Int 1))),
               (Ast.Phase2.Let ("y", (Ast.Phase2.Abs ("y", (Ast.Phase2.Int 2))),
                  (Ast.Phase2.Int 3)))
               ))
      apply 1: (Ast.Phase2.App ((Ast.Phase2.Var "a"), (Ast.Phase2.Var "b")))
      apply 2: (Ast.Phase2.App (
                  (Ast.Phase2.App ((Ast.Phase2.Var "a"), (Ast.Phase2.Var "b"))),
                  (Ast.Phase2.Var "c"))) |}]
