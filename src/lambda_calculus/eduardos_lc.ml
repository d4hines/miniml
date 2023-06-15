(* Adapted from https://github.com/EduardoRFS/youtube-channel/blob/master/07-lambda-calculus-interpreter-in-ocaml/code/tuturulang.ml *)

module Context = Map.Make (String)

type expr =
  | ENat of int
  | Variable of string
  | Abstraction of { param : string; body : expr }
  | Application of { funct : expr; argument : expr }
  | Let of { name : string; value : expr; body : expr }
  | EUnit
[@@deriving show]

type value =
  | VNat of int
  | Closure of {
      context : value Context.t; [@opaque]
      param : string;
      body : expr;
    }
  | Native of (value -> value)
  | VUnit
[@@deriving show]

let rec interp context expr =
  match expr with
  | ENat x -> VNat x
  | Variable name -> Context.find name context
  | Abstraction { param; body } -> Closure { context; param; body }
  | Let { name; value; body } ->
      let value = interp context value in
      interp (Context.add name value context) body
  | EUnit -> VUnit
  | Application { funct; argument } -> (
      let argument = interp context argument in
      match interp context funct with
      | VUnit -> VUnit
      | VNat x -> VNat x
      | Closure { context; param; body } ->
          interp (Context.add param argument context) body
      | Native f -> f argument)

let lambda var expr = Abstraction { param = var; body = expr }
let apply a b = Application { funct = a; argument = b }
let int x = ENat x
let var x = Variable x
let unit = EUnit
let true_ = lambda "x" (lambda "y" (apply (var "x") unit))
let false_ = lambda "x" (lambda "y" (apply (var "y") unit))
let true_v = interp Context.empty true_
let false_v = interp Context.empty false_

let initial_context =
  Context.empty
  |> Context.add "debug"
       (Native
          (fun v ->
            (match v with
            | VNat x -> Format.printf "debug: %d\n" x
            | v -> Format.printf "debug: %a\n" pp_value v);
            v))
  |> Context.add "succ"
       (Native
          (function
          | VNat x when x > 1000 -> failwith "aaaaaah too high"
          | VNat x ->
              print_endline "succ";
              VNat (x + 1)
          | x ->
              Format.printf "value: %a\n%!" pp_value x;
              failwith "succ: not an int"))
  |> Context.add "pred"
       (Native
          (function
          | VNat 0 -> VNat 0
          | VNat x -> VNat (x - 1)
          | x ->
              Format.printf "value: %a\n%!" pp_value x;
              failwith "pred: not an int:"))
  |> Context.add "zero?"
       (Native
          (function
          | VNat 0 -> true_v | VNat _ -> false_v | _ -> failwith "not an int"))

let if_e =
  lambda "condition"
    (lambda "consequent"
       (lambda "alternative"
          (apply
             (apply (var "condition") (var "consequent"))
             (var "alternative"))))

let let_ name value body = Let { name; value; body }

(** Does not work well in an eager language! *)
let y =
  lambda "f"
    (let_ "g"
       (lambda "h"
          (lambda "x"
             (apply (apply (var "f") (apply (var "h") (var "h"))) (var "x"))))
       (apply (var "g") (var "g")))

(* const Z = g => (x => g(v => x(x)(v)))(x => g(v => x(x)(v))) *)
let z =
  lambda "g"
    (apply
       (lambda "x"
          (apply (var "g")
             (lambda "v" (apply (apply (var "x") (var "x")) (var "v")))))
       (lambda "x"
          (apply (var "g")
             (lambda "v" (apply (apply (var "x") (var "x")) (var "v"))))))

let if_ condition consequent alternative =
  apply (apply (apply if_e condition) consequent) alternative

let succ a = apply (var "succ") a
let pred a = apply (var "pred") a
let is_zero a = apply (var "zero?") a
let debug a = apply (var "debug") a
let apply2 f a b = apply (apply f a) b

let plus_e =
  apply z
    (lambda "h"
       (lambda "a"
          (lambda "b"
             (if_
                (is_zero (debug (var "b")))
                (lambda "_" (var "a"))
                (lambda "_"
                   (apply2 (var "h") (succ (var "a")) (pred (var "b"))))))))

let thunk a = lambda "_" a

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
let print_expr label value = Format.printf "%s = %a\n%!" label pp_value value
let eval = interp initial_context

let%expect_test "eval" =
  (* print_expr "ex1" @@ eval (lambda "x" (lambda "y" (int 1)));
     print_expr "ex2" @@ eval (apply (lambda "x" (var "x")) (int 2));
     print_expr "printing ints" @@ eval (apply (var "debug") (int 4));
     print_expr "succ" @@ eval (apply (var "succ") (int 5));
     print_expr "let"
     @@ eval (let_ "magic_num" (int 6) (apply (var "succ") (var "magic_num"))); *)
  (* print_expr "random y stuff"
     @@ eval
          (apply
             (apply y (lambda "h" (lambda "n" (apply (var "h") (succ (var "n"))))))
             (int 0)); *)
  print_expr "is_zero true" @@ eval (if_ (is_zero (int 0)) (int 1) (int 2));
  print_expr "is_zero false" @@ eval (if_ (is_zero (int 1)) (int 1) (int 2));
  print_expr "pred 3" @@ eval (pred (int 3));
  print_expr "pred 0" @@ eval (pred (int 0));
  print_expr "succ 0" @@ eval (succ (int 0));
  print_expr "succ 1" @@ eval (succ (int 1));
  print_expr "debug test" @@ eval (debug (int 4));
  print_expr "plus" @@ eval (int 2 + int 1);
  print_expr "minus" @@ eval (int 4 - int 3);
  (* print_expr "ex2"
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
       (eval (apply (apply (apply if_ true_) (var "foo")) (var "bar"))); *)
  [%expect
    {|
    is_zero true = (Eduardos_lc.VNat 1)
    is_zero false = (Eduardos_lc.VNat 2)
    pred 3 = (Eduardos_lc.VNat 2)
    pred 0 = (Eduardos_lc.VNat 0)
    succ
    succ 0 = (Eduardos_lc.VNat 1)
    succ
    succ 1 = (Eduardos_lc.VNat 2)
    debug: 4
    debug test = (Eduardos_lc.VNat 4)
    succ
    debug: 1
    debug: 0
    plus = (Eduardos_lc.VNat 3)
    minus = (Eduardos_lc.VNat 1) |}]
