%{

open Ast

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
// hack for multiple application
| LPAREN e1 = expr e2 = expr e3 = expr RPAREN
    { App (App (e1, e2), e3) }   
| LPAREN e1 = expr e2 = expr e3 = expr e4 = expr RPAREN
    { App (App (App (e1, e2), e3), e4) }      
| LPAREN e1 = expr e2 = expr e3 = expr e4 = expr e5 = expr RPAREN
    { App (App (App (App (e1, e2), e3), e4), e5) }         
| LPAREN e = expr RPAREN
    { e }
| FUN x = VAR ARROW body = expr { Abs (x, body) }
| LET x = VAR EQ e1 = expr IN e2 = expr { Let (x, e1, e2)} 
| v  = VAR { Var v }
