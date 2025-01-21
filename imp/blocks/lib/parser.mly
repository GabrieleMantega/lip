%{
open Ast
%}

/****************************/
/*****     Tokens       *****/
/****************************/
/* values */
%token <int> CONST
%token TRUE
%token FALSE
/* operators */
%token ADD
%token SUB
%token MUL
%token NOT
%token OR
%token AND
%token EQ
%token LEQ
/* selection construct */
%token IF
%token THEN
%token ELSE
/* iteration construct */
%token WHILE
%token DO
/* declaration */
%token INTVAR
%token BOOLVAR
/* miscellaneous */
%token SKIP
%token SEQ
%token LPAREN
%token RPAREN
%token LCPAREN
%token RCPAREN
/* variables */
%token ASSIGN
%token <string> IDE
/* end of file */
%token EOF

/****************************/
/*****  Associativity   *****/
/****************************/

%left SEQ
%nonassoc ELSE DO 
%left OR AND
%nonassoc NOT
%left LEQ EQ 
%left ADD SUB
%left MUL

/****************************/
/*****   Productions    *****/
/****************************/
%start <cmd> prog

%%

prog:
  | c=cmd; EOF { c }
;

ide:
  | i=IDE { i }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr {Not e}
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e=CONST; { Const(e) } 
  | e1=expr; ADD; e2=expr { Add(e1,e2) }
  | e1=expr; SUB; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | LPAREN; e=expr; RPAREN { e }
  | i=ide { Var(i) }
;

decl:
  | INTVAR; i=ide; SEQ { IntVar(i) }
  | BOOLVAR; i=ide; SEQ { BoolVar(i) }
;

cmd:
  | SKIP { Skip }
  | LPAREN; c=cmd; RPAREN { c }
  | i=ide; ASSIGN; e=expr { Assign(i,e) }
  | c1=cmd; SEQ; c2=cmd { Seq(c1,c2) }
  | IF; e=expr; THEN; c1=cmd; ELSE; c2=cmd { If(e,c1,c2) }
  | WHILE; e=expr; DO; c=cmd { While(e,c) }
  | LCPAREN; dl=list(decl); c=cmd; RCPAREN { Decl(dl,c) }
;