(* Translating https://matt.might.net/articles/cek-machines/ from Haskell to OCaml *)
type expr = Lam of lambda | Var of string | App of expr * expr
and lambda = string * expr

let church_numeral_to_decimal expr =
  match expr with
  | Lam (f, Lam (x, expr)) when f != x ->
      let rec count_apps expr acc =
        match expr with
        | App (Var var, expr) ->
            if var == f then count_apps expr (acc + 1) else None
        | Var v when v == x -> Some acc
        | _ -> None
      in
      count_apps expr 0
  | _ -> None

let rec pp_lambda : Format.formatter -> lambda -> unit =
 fun fmt (var, expr) -> Format.fprintf fmt "\\%s.%a" var pp_expr expr

and pp_expr fmt expr =
  match church_numeral_to_decimal expr with
  | Some x -> Format.fprintf fmt "Church(%d)" x
  | None -> (
      match expr with
      | Lam lambda -> pp_lambda fmt lambda
      | Var x -> Format.fprintf fmt "%s" x
      | App (e1, e2) -> Format.fprintf fmt "(%a %a)" pp_expr e1 pp_expr e2)

module Environment = Map.Make (String)

type env = data Environment.t
and data = Clo of lambda * env

let rec pp_data fmt data =
  match data with
  | Clo (lambda, env) ->
      Format.fprintf fmt "Clo (%a, %a)" pp_lambda lambda pp_env env

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
  [%expect {| Clo (\x.x, {id => Clo (\y.y, {});}) |}]

type kont =
  | (* The empty continuation *) Mt
  | (* The "I hold an argument to evaluate" continuation*) Ar of
      expr * env * kont
  | (* The "I contain an evaluated function, now I'm evaluating an argument term" continuation *)
    Fn of
      lambda * env * kont
[@@deriving show]

type state = expr * env * kont [@@deriving show]

let counter = ref 0

let get_counter () =
  let x = !counter in
  counter := x + 1;
  x

let step (state : state) : state =
  (* Format.printf "state %d: %a\n---------------------------------\n%!"
     (get_counter ()) pp_state state; *)
  match state with
  (* Evaluating a reference? Look it up in the environment. *)
  | Var x, env, k -> (
      match Environment.find_opt x env with
      | Some (Clo (lam, env')) ->
          (Lam lam, env', k)
          (* This seems a bit wrong to me. We're going backwords from data to ast... *)
      | None -> failwith @@ "unable to find variable " ^ x)
  (* Evaluating a function application? First evaluate the function. *)
  | App (f, e), env, k -> (f, env, Ar (e, env, k))
  (* Evaluated the function? Go evaluate the argument term. *)
  | Lam lam, env, Ar (e, env', k) -> (e, env', Fn (lam, env, k))
  (* Evaluated the argument too? Perform the application. *)
  | Lam lam, env, Fn ((x, e), env', k) ->
      let new_env = Environment.add x (Clo (lam, env)) env' in
      (e, new_env, k)
  | Lam _, _, Mt -> state

let is_final state = match state with Lam _, _, Mt -> true | _ -> false

let rec terminal_whnf : state -> state =
 fun s -> if is_final s then s else terminal_whnf (step s)

(* let rec eval_normalize state acc = *)
(* let state = terminal_whnf state in
   if state == acc then acc
   else
     match state with
     | Lam (var, expr), env, Mt ->
         Format.printf "expr: %a\n%!" pp_expr expr;
         Format.printf "env: %a\n%!" pp_env env;
         let expr', _env, _ = eval_normalize (expr, env, Mt) state in
         (Lam (var, expr'), env, Mt)
     | _ -> assert false *)

let inject (e : expr) : state =
  let empty_env = Environment.empty in
  (e, empty_env, Mt)

let eval_normalize expr =
  match terminal_whnf (inject expr) with
  | Lam (_var, _expr), env, Mt -> (
      let () = Format.printf "_expr: %a\n%!" pp_expr _expr in
      let () = Format.printf "env: %a\n%!" pp_env env in
      match terminal_whnf (_expr, env, Mt) with
      | expr, _env', _ -> (Lam (_var, expr), env, Mt))
  | _ -> assert false

let eval expr = terminal_whnf (inject expr)
let initial_environment = Environment.empty
let lambda var expr = Lam (var, expr)
let apply a b = App (a, b)
let apply2 f a b = apply (apply f a) b
let var x = Var x
let id_e = lambda "x" (var "x")
let id x = apply id_e x
let thunk a = lambda "_" a

module Church_numerals = struct
  let zero = lambda "f" (lambda "x" (var "x"))
  let one = lambda "f" (lambda "x" (apply (var "f") (var "x")))

  let plus_e =
    lambda "m"
      (lambda "n"
         (lambda "f"
            (lambda "x"
               (apply2 (var "m") (var "f")
                  (apply2 (var "n") (var "f") (var "x"))))))

  let succ_e =
    lambda "n"
      (lambda "f"
         (lambda "x"
            (apply (apply (apply plus_e (var "n")) (var "f")) (var "x"))))

  let unit = zero
  let true_ = lambda "a" (lambda "b" (apply (var "a") unit))
  let false_ = lambda "a" (lambda "b" (apply (var "b") unit))

  let if_e =
    lambda "pred"
      (lambda "a" (lambda "b" (apply2 (var "pred") (var "a") (var "b"))))

  let if_ cond a b = apply (apply2 if_e cond a) b
  let is_zero = lambda "n" (apply (thunk false_) true_)

  let pred_e =
    let true_ = lambda "a" (lambda "b" (var "a")) in
    let false_ = lambda "a" (lambda "b" (var "b")) in
    let pair =
      lambda "x"
        (lambda "y" (lambda "f" (apply2 (var "f") (var "x") (var "y"))))
    in
    let fst = lambda "p" (apply (var "p") true_) in
    let snd = lambda "p" (apply (var "p") false_) in
    let shift =
      lambda "p"
        (apply2 pair (apply snd (var "p")) (apply succ_e (apply snd (var "p"))))
    in
    lambda "n" (apply fst (apply2 (var "n") shift (apply2 pair zero zero)))

  let succ x = apply succ_e x
  let pred x = apply pred_e x

  let z =
    lambda "g"
      (let h =
         lambda "x"
           (apply (var "g") (lambda "v" (apply2 (var "x") (var "x") (var "v"))))
       in
       apply h h)
end

let print_result label state = Format.printf "%s = %a\n%!" label pp_state state

let%expect_test "eval" =
  let open Church_numerals in
  (* let two =
       lambda "z" (lambda "z'" (apply (var "z") (apply (var "z") (var "z'"))))
     in *)
  (* print_result "one" @@ eval @@ id one;
     print_result "two" @@ eval @@ two; *)
  print_result "succ zero" @@ eval_normalize @@ succ zero;
  [%expect
    {|
    _expr: \x.(((\m.\n.\f.\x.((m f) ((n f) x)) n) f) x)
    env: {n => Clo (\f.\x.x, {});}
    succ zero = (\f.\x.(((\m.\n.\f.\x.((m f) ((n f) x)) n) f) x),
                 {n => Clo (\f.\x.x, {});}, Cek.Mt) |}]
