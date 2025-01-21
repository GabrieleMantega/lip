open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let getNat = function
  Nat x -> x
 | _ -> raise @@ TypeError("Nat type expected")
;;

let getBool = function
  Bool x -> x
 | _ -> raise @@ TypeError("Bool type expected")
;;

let rec eval_expr (st:state) = function
    True -> Bool true
  | False -> Bool false
  | Const(n) -> Nat n
  | Not(e) -> Bool(not @@ getBool @@ eval_expr st e)  
  | And(e1,e2) -> Bool((getBool @@ eval_expr st e1) && (getBool @@ eval_expr st e2))
  | Or(e1,e2) -> Bool((getBool @@ eval_expr st e1) || (getBool @@ eval_expr st e2))
  | Add(e1,e2) -> Nat((getNat @@ eval_expr st e1) + (getNat @@ eval_expr st e2))
  | Sub(e1,e2) -> 
    let natval = Nat((getNat @@ eval_expr st e1) - (getNat @@ eval_expr st e2)) in
    if natval >= Nat 0 then natval
    else raise @@ TypeError "Only Nat values allowed (Negative numbers encountered)" 
  | Mul(e1,e2) -> Nat((getNat @@ eval_expr st e1) * (getNat @@ eval_expr st e2))
  | Eq(e1,e2) -> Bool(eval_expr st e1 = eval_expr st e2)
  | Leq(e1,e2) -> Bool((getNat @@ eval_expr st e1) <= (getNat @@ eval_expr st e2))
  | Var(x) -> (st x)
;;

let bottom:state = (fun id -> raise @@ UnboundVar("Unbound var '"^id^"'"));;

let bind (st:state) (x:ide) (v:exprval) = ((fun id -> if id=x then v else st id):state);;

let rec trace1 = function
  Cmd(cmd,st) -> 
    (match cmd with
        Skip -> St st
      | Assign(x,v) -> St (bind st x (eval_expr st v))
      | Seq(c1,c2) -> 
        (let res = (trace1 @@ Cmd(c1,st)) in
          match res with
            St st' -> Cmd(c2,st')
          | Cmd(c1',st') -> Cmd(Seq(c1',c2),st'))
      | If(e,c1,c2) -> if (eval_expr st e = Bool true) then Cmd(c1,st) else Cmd(c2,st)
      | While(e,c) -> if (eval_expr st e = Bool true) then Cmd(Seq(c,While(e,c)), st) else St st)
  | St _ -> raise NoRuleApplies
;;


let trace n cmd =
  let bottom:state = (fun id -> raise @@ UnboundVar("Unbound var '"^id^"'")) in
  let rec rectrace n conf =
    if n>0 then
      try 
        let step = trace1 conf in
        conf::(rectrace (n-1) step) 
      with NoRuleApplies -> [conf]
    else []
  in

  rectrace n (Cmd(cmd,bottom))
;;

let rec getstate = function
    [] -> failwith "empty conf-list"
  | [x] -> 
    (match x with
      St st -> st
      | _ -> failwith "last conf of conf-list is not a state")
  | _::l -> getstate l
;;