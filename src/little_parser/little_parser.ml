let () =
  let lexbuf = Lexing.from_string "2+2*3;" in
  let x = Parser.main Lexer.token lexbuf in
  List.iter (fun x -> Format.printf "result: %d" x) x
