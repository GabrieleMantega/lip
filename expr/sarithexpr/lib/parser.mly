%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token EOF
%token NOT
%token AND
%token OR
%token ZERO
%token SUCC
%token PRED
%token ISZERO

%nonassoc ELSE
%left OR
%left AND
%nonassoc NOT
%nonassoc SUCC PRED ISZERO 

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | NOT; e=expr; {Not e}
  | e1 = expr; AND; e2 = expr; { And(e1,e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1,e2) }
  | ZERO {Zero}
  | SUCC; e = expr {Succ(e)}
  | PRED; e = expr {Pred(e)}
  | ISZERO; e = expr {IsZero(e)}
  | LPAREN; e=expr; RPAREN {e}
;