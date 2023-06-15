(* must use a thunk for compatibility with recursion! *)
let true_ a _ = a ()
let false_ _a b = b ()
let if_ a b c = a b c
let succ x = x + 1
let is_zero x = if x = 0 then true_ else false_
let pred x = if x = 0 then 0 else x - 1

type 'a fix = Fix of ('a fix -> 'a)

let z f =
  let g (Fix x as fx) = f (fun v -> x fx v) in
  g (Fix g)

let plus =
  z (fun h a b -> if_ (is_zero b) (fun () -> a) (fun () -> h (succ a) (pred b)))

let%expect_test "im stupid tests" =
  let result = plus 5 7 in
  Format.printf "result: %d" result;
  [%expect {| result: 12 |}]
