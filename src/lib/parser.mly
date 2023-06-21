%{

open Ast.Phase2

%}
/* taken from https://github.com/bryphe/hello-menhir */
%token <int> INT
%token <string> VAR
%token FUN
%token ARROW
%token LET
%token IN
%token EQ
%token LPAREN RPAREN
%token EOF

%start <expr> expr

%%

expr:
| i = INT
    { Int i }
| LPAREN e1 = expr e2 = expr RPAREN
    { App (e1, e2) }
| FUN x = VAR ARROW body = expr { Abs (x, body) }
| LET x = VAR EQ e1 = expr IN e2 = expr { Let (x, e1, e2)} 
| v  = VAR { Var v }
