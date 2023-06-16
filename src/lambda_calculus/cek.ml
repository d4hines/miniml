(* Translating https://matt.might.net/articles/cek-machines/ from Haskell to OCaml *)
type expr =
  | Lam of lambda
  | Var of string
  | App of expr * expr
  | Int of int
  | Succ of expr
  | Pred of expr
  | IsZero of expr

and lambda = string * expr [@@deriving show]

module Environment = Map.Make (String)

type env = data Environment.t
and data = Clo of lambda * env | IntV of int

let rec pp_data fmt data =
  match data with
  | Clo (lambda, env) ->
      Format.fprintf fmt "Clo (%a, %a)" pp_lambda lambda pp_env env
  | IntV x -> Format.fprintf fmt "%d" x

and pp_env fmt env =
  let () = Format.fprintf fmt "{" in
  List.iter
    (fun (k, v) -> Format.fprintf fmt "%s => %a;" k pp_data v)
    (Environment.bindings env);
  Format.fprintf fmt "}"

let%expect_test "data" =
  let data =
    Clo
      ( ("x", Var "x"),
        Environment.empty
        |> Environment.add "id" (Clo (("y", Var "y"), Environment.empty)) )
  in
  Format.printf "%a" pp_data data;
  [%expect
    {| Clo (("x", (Cek.Var "x")), {id => Clo (("y", (Cek.Var "y")), {});}) |}]

type kont =
  | (* The empty continuation *) Mt
  | (* The "I hold an argument to evaluate" continuation*) Ar of
      expr * env * kont
  | (* The "I contain an evaluated function, now I'm evaluating an argument term" continuation *)
    Fn of
      lambda * env * kont
  | IsZeroK of kont
  | SuccK of kont
  | PredK of kont
[@@deriving show]

type state = expr * env * kont [@@deriving show]

let inject (e : expr) : state =
  let empty_env = Environment.empty in
  (e, empty_env, Mt)

let church_true = Lam ("x", Lam ("y", Var "x"))
let church_false = Lam ("x", Lam ("y", Var "y"))
let counter = ref 0

let get_counter () =
  let x = !counter in
  counter := x + 1;
  x

let rec step (state : state) : state =
  (* Format.printf "state %d: %a\n---------------------------------\n%!"
     (get_counter ()) pp_state state; *)
  match state with
  (* Evaluating a reference? Look it up in the environment. *)
  | Var x, env, k -> (
      match Environment.find x env with
      | Clo (lam, env') -> (Lam lam, env', k)
      | IntV x ->
          (Int x, env, k)
          (* This seems a bit wrong to me. We're going backwords from data to ast... *)
      )
  (* Evaluating a function application? First evaluate the function. *)
  | App (f, e), env, k -> (f, env, Ar (e, env, k))
  (* Evaluated the function? Go evaluate the argument term. *)
  | Lam lam, env, Ar (e, env', k) -> (e, env', Fn (lam, env, k))
  (* Evaluated the argument too? Perform the application. *)
  | Lam lam, env, Fn ((x, e), env', k) ->
      let new_env = Environment.add x (Clo (lam, env)) env' in
      (e, new_env, k)
  | (Int i, env, k) as state -> (
      match k with
      | IsZeroK k ->
          let bool = if i = 0 then church_true else church_false in
          step (bool, env, k)
      | SuccK k -> step (Int (i + 1), env, k)
      | PredK k ->
          let i = if i = 0 then 0 else i - 1 in
          step (Int i, env, k)
      | Fn ((x, e), env', k) ->
          let new_env = Environment.add x (IntV i) env' in
          step (e, new_env, k)
          (* let x = (e, new_env, k) in
             Format.printf "returning state: %a" pp_state x; *)
      | Ar _ ->
          Format.printf "State: %a\n%!" pp_state state;
          failwith "Cannot apply an argument to an integer"
      | Mt -> state)
  | IsZero e, env, k -> (e, env, IsZeroK k)
  | Succ e, env, k -> (e, env, SuccK k)
  | Pred e, env, k -> (e, env, PredK k)
  | Lam _, _, Mt -> state
  | Lam _, _, SuccK _ | Lam _, _, PredK _ | Lam _, _, IsZeroK _ ->
      failwith "Invalid transition"

let is_final state = match state with _, _, Mt -> true | _ -> false

let rec eval i state =
  if i > 5000 then failwith "evaluation too deep";
  match step state with
  | result when not @@ is_final result -> eval (i + 1) result
  | Var x, env, _ -> Environment.find x env
  | Int x, _, _ -> IntV x
  | state -> eval (i + 1) state
(* let state_str = show_state state in
   failwith @@ "unexpected final state: " ^ state_str *)

let initial_environment = Environment.empty
let eval expr = eval 0 (expr, initial_environment, Mt)
let lambda var expr = Lam (var, expr)
let apply a b = App (a, b)
let int x = Int x
let var x = Var x
let unit = Int 0
let true_ = lambda "x" (lambda "y" (apply (var "x") unit))
let false_ = lambda "x" (lambda "y" (apply (var "y") unit))
let succ a = Succ a
let pred a = Pred a
let is_zero a = IsZero a
let apply2 f a b = apply (apply f a) b

let z =
  lambda "g"
    (apply
       (lambda "x"
          (apply (var "g")
             (lambda "v" (apply (apply (var "x") (var "x")) (var "v")))))
       (lambda "x"
          (apply (var "g")
             (lambda "v" (apply (apply (var "x") (var "x")) (var "v"))))))

let if_e =
  lambda "condition"
    (lambda "consequent"
       (lambda "alternative"
          (apply
             (apply (var "condition") (var "consequent"))
             (var "alternative"))))

let if_ condition consequent alternative =
  apply (apply (apply if_e condition) consequent) alternative

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
let print_result label value = Format.printf "%s = %a\n%!" label pp_data value

let%expect_test "eval" =
  print_result "id" @@ eval
  @@ apply (lambda "x" (var "x")) (lambda "hello" (Var "hello"));
  print_result "id with ints" @@ eval @@ apply (lambda "x" (var "x")) (int 1);
  print_result "int should eval to int" @@ eval (int 3);
  print_result "is_zero 0" @@ eval (apply2 (is_zero (int 0)) (int 1) (int 0));
  print_result "is_zero 3" @@ eval (apply2 (is_zero (int 3)) (int 1) (int 0));
  print_result "is_zero true"
  @@ eval (apply (apply2 if_e church_true (int 1)) (int 0));
  print_result "is_zero false" @@ eval (if_ (is_zero (int 1)) (int 1) (int 0));
  print_result "pred 3" @@ eval (pred (int 3));
  print_result "pred 0" @@ eval (pred (int 0));
  print_result "succ 0" @@ eval (succ (int 0));
  print_result "succ 1" @@ eval (succ (int 1));
  (* print_result "plus" @@ eval (int 2 + int 1); *)
  (* print_result "minus" @@ eval (int 4 - int 3); *)
  (*
      
      
       *)
  [%expect
    {|
    id = Clo (("hello", (Cek.Var "hello")), {})
    id with ints = 1
    int should eval to int = 3
    is_zero 0 = 1
    is_zero 3 = 0
    is_zero true = 1
    is_zero false = 0
    pred 3 = 2
    pred 0 = 0
    succ 0 = 1
    succ 1 = 2 |}]
