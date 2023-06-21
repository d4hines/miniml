
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

open Ast.Phase2

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
    (Ast.Phase2.Let ("x", (Ast.Phase2.Abs ("x", (Ast.Phase2.Int 1))),
       (Ast.Phase2.Let ("y", (Ast.Phase2.Abs ("y", (Ast.Phase2.Int 2))),
          (Ast.Phase2.Int 3)))
       )) |}]

let%expect_test "apply 1" =
  show_ast {|(a b)|};
  [%expect {| (Ast.Phase2.App ((Ast.Phase2.Var "a"), (Ast.Phase2.Var "b"))) |}]

let%expect_test "apply 2" =
  show_ast {|(a b c)|};
  [%expect
    {|
    (Ast.Phase2.App (
       (Ast.Phase2.App ((Ast.Phase2.Var "a"), (Ast.Phase2.Var "b"))),
       (Ast.Phase2.Var "c"))) |}]

let%expect_test "z combinator" =
  show_ast
    {| fun f ->
         let g = fun x -> (f (fun v -> ((x x) v))) in
         (g g) |};
  [%expect
    {|
    (Ast.Phase2.Abs ("f",
       (Ast.Phase2.Let ("g",
          (Ast.Phase2.Abs ("x",
             (Ast.Phase2.App ((Ast.Phase2.Var "f"),
                (Ast.Phase2.Abs ("v",
                   (Ast.Phase2.App (
                      (Ast.Phase2.App ((Ast.Phase2.Var "x"), (Ast.Phase2.Var "x")
                         )),
                      (Ast.Phase2.Var "v")))
                   ))
                ))
             )),
          (Ast.Phase2.App ((Ast.Phase2.Var "g"), (Ast.Phase2.Var "g")))))
       )) |}]

let%expect_test "plus" =
  show_ast
    {|(z (fun h -> fun a -> fun b ->
           (if_ (is_zero b)
             (fun _ -> a)
             (fun _ -> (h (succ a) (pred b))))))|};
  [%expect
    {|
    (Ast.Phase2.App ((Ast.Phase2.Var "z"),
       (Ast.Phase2.Abs ("h",
          (Ast.Phase2.Abs ("a",
             (Ast.Phase2.Abs ("b",
                (Ast.Phase2.App (
                   (Ast.Phase2.App (
                      (Ast.Phase2.App ((Ast.Phase2.Var "if_"),
                         (Ast.Phase2.App ((Ast.Phase2.Var "is_zero"),
                            (Ast.Phase2.Var "b")))
                         )),
                      (Ast.Phase2.Abs ("_", (Ast.Phase2.Var "a"))))),
                   (Ast.Phase2.Abs ("_",
                      (Ast.Phase2.App (
                         (Ast.Phase2.App ((Ast.Phase2.Var "h"),
                            (Ast.Phase2.App ((Ast.Phase2.Var "succ"),
                               (Ast.Phase2.Var "a")))
                            )),
                         (Ast.Phase2.App ((Ast.Phase2.Var "pred"),
                            (Ast.Phase2.Var "b")))
                         ))
                      ))
                   ))
                ))
             ))
          ))
       )) |}]

let%expect_test "stdlib parse" =
  let stdlib = read_file "./examples/stdlib.txt" in
  (* stdlib is missing a final expression *)
  let expr = stdlib ^ "\n1" in
  show_ast expr;
  [%expect
    {|
        (Ast.Phase2.Let ("true_",
           (Ast.Phase2.Abs ("x", (Ast.Phase2.Abs ("y", (Ast.Phase2.Var "x"))))),
           (Ast.Phase2.Let ("false",
              (Ast.Phase2.Abs ("x", (Ast.Phase2.Abs ("y", (Ast.Phase2.Var "y"))))),
              (Ast.Phase2.Let ("if_",
                 (Ast.Phase2.Abs ("condition",
                    (Ast.Phase2.Abs ("consequent",
                       (Ast.Phase2.Abs ("alternative",
                          (Ast.Phase2.App (
                             (Ast.Phase2.App ((Ast.Phase2.Var "condition"),
                                (Ast.Phase2.Var "consequent"))),
                             (Ast.Phase2.Var "alternative")))
                          ))
                       ))
                    )),
                 (Ast.Phase2.Let ("z",
                    (Ast.Phase2.Abs ("f",
                       (Ast.Phase2.Let ("g",
                          (Ast.Phase2.Abs ("x",
                             (Ast.Phase2.App ((Ast.Phase2.Var "f"),
                                (Ast.Phase2.Abs ("v",
                                   (Ast.Phase2.App (
                                      (Ast.Phase2.App ((Ast.Phase2.Var "x"),
                                         (Ast.Phase2.Var "x"))),
                                      (Ast.Phase2.Var "v")))
                                   ))
                                ))
                             )),
                          (Ast.Phase2.App ((Ast.Phase2.Var "g"), (Ast.Phase2.Var "g")
                             ))
                          ))
                       )),
                    (Ast.Phase2.Let ("plus",
                       (Ast.Phase2.App ((Ast.Phase2.Var "z"),
                          (Ast.Phase2.Abs ("h",
                             (Ast.Phase2.Abs ("a",
                                (Ast.Phase2.Abs ("b",
                                   (Ast.Phase2.App (
                                      (Ast.Phase2.App (
                                         (Ast.Phase2.App ((Ast.Phase2.Var "if_"),
                                            (Ast.Phase2.App (
                                               (Ast.Phase2.Var "is_zero"),
                                               (Ast.Phase2.Var "b")))
                                            )),
                                         (Ast.Phase2.Abs ("_", (Ast.Phase2.Var "a")))
                                         )),
                                      (Ast.Phase2.Abs ("_",
                                         (Ast.Phase2.App (
                                            (Ast.Phase2.App ((Ast.Phase2.Var "h"),
                                               (Ast.Phase2.App (
                                                  (Ast.Phase2.Var "succ"),
                                                  (Ast.Phase2.Var "a")))
                                               )),
                                            (Ast.Phase2.App ((Ast.Phase2.Var "pred"),
                                               (Ast.Phase2.Var "b")))
                                            ))
                                         ))
                                      ))
                                   ))
                                ))
                             ))
                          )),
                       (Ast.Phase2.Int 1)))
                    ))
                 ))
              ))
           )) |}]
