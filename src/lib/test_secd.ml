open Ast.Phase0
open Secd

let lambda var expr = Abs (var, expr)
let var v = Var v
let apply a b = App (a, b)
let apply2 f a b = apply (apply f a) b
let int x = Int x
let unit = Int 0
let true_ = lambda "x" (lambda "y" (apply (var "x") unit))
let false_ = lambda "x" (lambda "y" (apply (var "y") unit))
let succ x = Succ x
let pred x = Pred x
let is_zero x = IsZero x

let if_e =
  lambda "condition"
    (lambda "consequent"
       (lambda "alternative"
          (apply
             (apply (var "condition") (var "consequent"))
             (var "alternative"))))

let if_ condition consequent alternative =
  apply (apply (apply if_e condition) consequent) alternative

let z =
  lambda "g"
    (apply
       (lambda "x"
          (apply (var "g")
             (lambda "v" (apply (apply (var "x") (var "x")) (var "v")))))
       (lambda "x"
          (apply (var "g")
             (lambda "v" (apply (apply (var "x") (var "x")) (var "v"))))))

let thunk a = lambda "_" a

let plus_e =
  apply z
    (lambda "h"
       (lambda "a"
          (lambda "b"
             (if_
                (is_zero (var "b"))
                (thunk (var "a"))
                (thunk (apply2 (var "h") (succ (var "a")) (pred (var "b"))))))))

let minus_e =
  apply z
    (lambda "h"
       (lambda "a"
          (lambda "b"
             (if_
                (is_zero (var "b"))
                (thunk (var "a"))
                (thunk (apply2 (var "h") (pred (var "a")) (pred (var "b"))))))))

let ( + ) a b = apply2 plus_e a b
let ( - ) a b = apply2 minus_e a b

let run_code label code =
  let control = compile code in
  let result = execute control in
  Format.printf "%s: %a\n%!" label pp_answer result

let%expect_test "secd tests" =
  run_code "true 0 1" @@ apply2 true_ (thunk (int 0)) (thunk (int 1));
  run_code "false 0 1" @@ apply2 false_ (thunk (int 0)) (thunk (int 1));
  run_code "succ 1" @@ succ (int 1);
  run_code "pred 1" @@ pred (int 1);
  run_code "pred 0" @@ pred (int 0);
  run_code "is_zero" @@ if_ (is_zero (int 0)) (thunk (int 1)) (thunk (int 0));
  run_code "plus_z 1 2" @@ (int 1 + int 2);
  run_code "close: true" @@ true_;
  run_code "close: false" @@ false_;
  [%expect
    {|
    true 0 1: (Secd.I 0)
    false 0 1: (Secd.I 1)
    succ 1: (Secd.I 2)
    pred 1: (Secd.I 0)
    pred 0: (Secd.I 0)
    is_zero: (Secd.I 1)
    plus_z 1 2: (Secd.I 3)
    close: true: (Secd.Vclos ([], "x",
                    [(Secd.CLOS ("y",
                        [(Secd.LOOKUP "x"); (Secd.INT 0); Secd.CALL; Secd.RET]));
                      Secd.RET]
                    ))
    close: false: (Secd.Vclos ([], "x",
                     [(Secd.CLOS ("y",
                         [(Secd.LOOKUP "y"); (Secd.INT 0); Secd.CALL; Secd.RET]));
                       Secd.RET]
                     )) |}]
