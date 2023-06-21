
{
  (* Taken from https://github.com/bryphe/hello-menhir *)
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n'] (* also ignore newlines, not only whitespace and tabs *)
    { token lexbuf }
| "in" { IN }
| "let" { LET }
| "fun" { FUN }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "->" { ARROW }
| ['a'-'z' '_']+ as var
   { VAR var}
| "=" { EQ }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
