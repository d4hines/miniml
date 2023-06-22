open Compiler
open Secd

let lambda expr = DAbs expr
let var v = DVar v
let apply a b = DApp (a, b)
let apply2 f a b = apply (apply f a) b
let int x = DInt x
let unit = DInt 0
let true_ = lambda (lambda (apply (var 1) unit))
let false_ = lambda (lambda (apply (var 0) unit))
let succ x = DSucc x
let pred x = DPred x
let is_zero x = DIsZero x
let if_e = lambda (lambda (lambda (apply (apply (var 2) (var 1)) (var 0))))

let if_ condition consequent alternative =
  apply (apply (apply if_e condition) consequent) alternative

(* let z =
   lambda "g"
     (apply
        (lambda "x"
           (apply (var "g")
              (lambda "v" (apply (apply (var "x") (var "x")) (var "v")))))
        (lambda "x"
           (apply (var "g")
              (lambda "v" (apply (apply (var "x") (var "x")) (var "v")))))) *)

(* let thunk a = lambda "_"a *)

(* let plus_e =
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
                   (thunk (apply2 (var "h") (pred (var "a")) (pred (var "b")))))))) *)

(* let ( + ) a b = apply2 plus_e a b
   let ( - ) a b = apply2 minus_e a b *)

let run_code label code =
  let control = compile code in
  let result = execute control in
  Format.printf "%s: %a\n%!" label pp_answer result

let%expect_test "secd tests" =
  run_code "true 0 1" @@ apply2 true_ (lambda (int 0)) (lambda (int 1));
  run_code "false 0 1" @@ apply2 false_ (lambda (int 0)) (lambda (int 1));
  run_code "succ 1" @@ succ (int 1);
  run_code "pred 1" @@ pred (int 1);
  run_code "pred 0" @@ pred (int 0);
  run_code "is_zero" @@ if_ (is_zero (int 0)) (lambda (int 1)) (lambda (int 0));
  (* run_code "plus_z 1 2" @@ (int 1 + int 2); *)
  run_code "close: true" @@ true_;
  run_code "close: false" @@ false_;
  run_code "let" @@ DLet (int 1, var 0);
  run_code "nested let" @@ DLet (int 1, DLet (int 2, var 1));
  [%expect
    {|
    true 0 1: (Secd.I 0)
    false 0 1: (Secd.I 1)
    succ 1: (Secd.I 2)
    pred 1: (Secd.I 0)
    pred 0: (Secd.I 0)
    is_zero: (Secd.I 1)
    close: true: (Secd.Vclos ([],
                    [(Secd.CLOS
                        [(Secd.LOOKUP 1); (Secd.INT 0); Secd.CALL; Secd.RET]);
                      Secd.RET]
                    ))
    close: false: (Secd.Vclos ([],
                     [(Secd.CLOS
                         [(Secd.LOOKUP 0); (Secd.INT 0); Secd.CALL; Secd.RET]);
                       Secd.RET]
                     ))
    let: (Secd.I 1)
    nested let: (Secd.I 1) |}]
