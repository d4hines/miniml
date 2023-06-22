let read_file filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (acc ^ "\n" ^ line)
    with End_of_file ->
      close_in ic;
      acc
  in
  loop ""

let stdlib = read_file "./examples/stdlib.txt"

open Ast

let show_ast x =
  let lexbuf = Lexing.from_string x in
  let result = Parser.expr Lexer.token lexbuf in
  Format.printf "%a\n%!" pp_expr result

let%expect_test "lets" =
  show_ast {|
    let x = fun x -> 1 in
    let y = fun y -> 2 in
    3|};
  [%expect
    {|
    (Ast.Let ("x", (Ast.Abs ("x", (Ast.Int 1))),
       (Ast.Let ("y", (Ast.Abs ("y", (Ast.Int 2))), (Ast.Int 3))))) |}]

let%expect_test "apply 1" =
  show_ast {|(a b)|};
  [%expect {| (Ast.App ((Ast.Var "a"), (Ast.Var "b"))) |}]

let%expect_test "apply 2" =
  show_ast {|(a b c)|};
  [%expect
    {|
    (Ast.App ((Ast.App ((Ast.Var "a"), (Ast.Var "b"))), (Ast.Var "c"))) |}]

let%expect_test "z combinator" =
  show_ast
    {| fun f ->
         let g = fun x -> (f (fun v -> ((x x) v))) in
         (g g) |};
  [%expect
    {|
    (Ast.Abs ("f",
       (Ast.Let ("g",
          (Ast.Abs ("x",
             (Ast.App ((Ast.Var "f"),
                (Ast.Abs ("v",
                   (Ast.App ((Ast.App ((Ast.Var "x"), (Ast.Var "x"))),
                      (Ast.Var "v")))
                   ))
                ))
             )),
          (Ast.App ((Ast.Var "g"), (Ast.Var "g")))))
       )) |}]

let%expect_test "plus" =
  show_ast
    {|(z (fun h -> fun a -> fun b ->
           (if_ (is_zero b)
             (fun _ -> a)
             (fun _ -> (h (succ a) (pred b))))))|};
  [%expect
    {|
    (Ast.App ((Ast.Var "z"),
       (Ast.Abs ("h",
          (Ast.Abs ("a",
             (Ast.Abs ("b",
                (Ast.App (
                   (Ast.App (
                      (Ast.App ((Ast.Var "if_"),
                         (Ast.App ((Ast.Var "is_zero"), (Ast.Var "b"))))),
                      (Ast.Abs ("_", (Ast.Var "a"))))),
                   (Ast.Abs ("_",
                      (Ast.App (
                         (Ast.App ((Ast.Var "h"),
                            (Ast.App ((Ast.Var "succ"), (Ast.Var "a"))))),
                         (Ast.App ((Ast.Var "pred"), (Ast.Var "b")))))
                      ))
                   ))
                ))
             ))
          ))
       )) |}]

let%expect_test "stdlib parse" =
  (* stdlib is missing a final expression *)
  let expr = stdlib ^ "\n1" in
  show_ast expr;
  [%expect
    {|
        (Ast.Let ("unit", (Ast.Int 0),
           (Ast.Let ("true_",
              (Ast.Abs ("x", (Ast.Abs ("y", (Ast.App ((Ast.Var "x"), (Ast.Int 0)))))
                 )),
              (Ast.Let ("false_",
                 (Ast.Abs ("x",
                    (Ast.Abs ("y", (Ast.App ((Ast.Var "y"), (Ast.Int 0))))))),
                 (Ast.Let ("if_",
                    (Ast.Abs ("condition",
                       (Ast.Abs ("consequent",
                          (Ast.Abs ("alternative",
                             (Ast.App (
                                (Ast.App ((Ast.Var "condition"),
                                   (Ast.Var "consequent"))),
                                (Ast.Var "alternative")))
                             ))
                          ))
                       )),
                    (Ast.Let ("z",
                       (Ast.Abs ("f",
                          (Ast.Let ("g",
                             (Ast.Abs ("x",
                                (Ast.App ((Ast.Var "f"),
                                   (Ast.Abs ("v",
                                      (Ast.App (
                                         (Ast.App ((Ast.Var "x"), (Ast.Var "x"))),
                                         (Ast.Var "v")))
                                      ))
                                   ))
                                )),
                             (Ast.App ((Ast.Var "g"), (Ast.Var "g")))))
                          )),
                       (Ast.Let ("plus",
                          (Ast.App ((Ast.Var "z"),
                             (Ast.Abs ("h",
                                (Ast.Abs ("a",
                                   (Ast.Abs ("b",
                                      (Ast.App (
                                         (Ast.App (
                                            (Ast.App ((Ast.Var "if_"),
                                               (Ast.App ((Ast.Var "is_zero"),
                                                  (Ast.Var "b")))
                                               )),
                                            (Ast.Abs ("_", (Ast.Var "a"))))),
                                         (Ast.Abs ("_",
                                            (Ast.App (
                                               (Ast.App ((Ast.Var "h"),
                                                  (Ast.App ((Ast.Var "succ"),
                                                     (Ast.Var "a")))
                                                  )),
                                               (Ast.App ((Ast.Var "pred"),
                                                  (Ast.Var "b")))
                                               ))
                                            ))
                                         ))
                                      ))
                                   ))
                                ))
                             )),
                          (Ast.Int 1)))
                       ))
                    ))
                 ))
              ))
           )) |}]

let show_debruijn x =
  let lexbuf = Lexing.from_string x in
  let result = Parser.expr Lexer.token lexbuf in
  let x = Compiler.to_debruijn result in
  Format.printf "debruijn: %a\n" Compiler.pp_debruijn_expr x

let%expect_test "id" =
  show_debruijn "fun x -> x";
  [%expect {| debruijn: (Compiler.DAbs (Compiler.DVar 0)) |}]

let%expect_test "let" =
  show_debruijn "let false_ = fun x -> fun y -> y in false_";
  [%expect
    {|
    debruijn: (Compiler.DLet ((Compiler.DAbs (Compiler.DAbs (Compiler.DVar 0))),
                 (Compiler.DVar 0))) |}]

let%expect_test "succ" =
  show_debruijn "fun z -> (succ 1)";
  [%expect {| debruijn: (Compiler.DAbs (Compiler.DSucc (Compiler.DInt 1))) |}]

let%expect_test "pred" =
  show_debruijn "fun z -> (pred 1)";
  [%expect {| debruijn: (Compiler.DAbs (Compiler.DPred (Compiler.DInt 1))) |}]

let%expect_test "debruijn" =
  show_debruijn "fun z -> (is_zero 1)";
  [%expect
    {|
    debruijn: (Compiler.DAbs (Compiler.DIsZero (Compiler.DInt 1))) |}]

let run_code label code =
  let lexbuf = Lexing.from_string code in
  let parsed = Parser.expr Lexer.token lexbuf in
  let debruijn = Compiler.to_debruijn parsed in
  let compiled = Compiler.compile debruijn in
  let result = Secd.execute compiled in
  Format.printf "%s: %a\n%!" label Secd.pp_answer result

let run_code_stdlib label code =
  let code = stdlib ^ "\n" ^ code in
  run_code label code

let%expect_test "secd tests" =
  run_code_stdlib "true 0 1" @@ "(true_ (fun _ -> 0) (fun _ -> 1))";
  run_code_stdlib "false 0 1" @@ "(false_ (fun _ -> 0) (fun _ -> 1))";
  run_code_stdlib "succ 1" @@ "(succ 1)";
  run_code_stdlib "pred 1" @@ "(pred 1)";
  run_code_stdlib "pred 0" @@ "(pred 0)";
  run_code_stdlib "is_zero" @@ "(if_ (is_zero 0) (fun _ -> 1) (fun _ -> 0))";
  run_code_stdlib "plus 1 2" @@ "(plus 1 2)";
  [%expect
    {|
    true 0 1: (Secd.I 0)
    false 0 1: (Secd.I 1)
    succ 1: (Secd.I 2)
    pred 1: (Secd.I 0)
    pred 0: (Secd.I 0)
    is_zero: (Secd.I 1)
    plus 1 2: (Secd.I 3) |}]
