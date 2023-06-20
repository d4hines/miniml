let message = "hello world"

let%expect_test "dummy test" =
  print_endline message;
  [%expect {|hello world |}]

let%expect_test "parsing tests" =
  let lexbuf = Lexing.from_string "2+2*3;" in
  let x = Parser.main Lexer.token lexbuf in
  List.iter (fun x -> Format.printf "result: %d" x) x;
  [%expect {| result: 8 |}]
