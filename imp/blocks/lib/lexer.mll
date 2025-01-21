{
open Parser
}

let white = [' ' '\t' '\n']+
let const = '-'?['1'-'9']+['0'-'9']+|['0'-'9'] (* values starting with zero (e.g. "001") are forbidden *)
let idestring = ['a'-'z' 'A'-'z' '_']+['a'-'z' 'A'-'z' '_' '0'-'9']*

rule read =
  parse
  (* whitespaces *)
  | white { read lexbuf }

  (* values *)
  | const { CONST(int_of_string @@ Lexing.lexeme lexbuf) }
  | "true" { TRUE }
  | "false" { FALSE }

  (* operators *)
  | "+" {ADD}
  | "-" {SUB}
  | "*" {MUL}
  | "not" {NOT}
  | "and" {AND}
  | "or" {OR}
  | "=" {EQ}
  | "<=" {LEQ}
  
  (* selection construct *)
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}

  (* iteration construct*)
  | "while" {WHILE}
  | "do" {DO}

  (* declaration *)
  | "int" {INTVAR}
  | "bool" {BOOLVAR}

  (* miscellaneous *)
  | "skip" {SKIP}
  | ";" {SEQ}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LCPAREN }
  | "}" { RCPAREN }

  (* variables *)
  | ":=" {ASSIGN}
  | idestring { IDE(Lexing.lexeme lexbuf) }
  
  (* end of file*)
  | eof { EOF }