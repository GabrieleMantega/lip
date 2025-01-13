open Ast

type exprval = Bool of bool | Nat of int;;
type exprtype = BoolT | NatT;;

exception NoRuleApplies
exception PredBelowZero
exception NotNatInNatFun
exception NotBoolInBoolFun
exception TypeError of string;;

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not(" ^ string_of_expr e ^ ")" 
  | And(e1,e2) -> "And(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | Or(e1,e2) -> "Or(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | Zero -> "0"
  | Succ(e) -> "Succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "Pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "IsZero(" ^ string_of_expr e ^ ")"
;;

let string_of_val = function
    Bool v -> string_of_bool v
  | Nat v -> string_of_int v
;;

let string_of_type = function
    BoolT -> "Bool"
  | NatT -> "Nat"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e 
  | _ -> false

let rec trace1 = function
  (* if *)
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If(trace1 e0,e1,e2)
  (* not *)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> Not(trace1 e)
  (* and *)
  | And(True,e2) -> e2  
  | And(False,_) -> False
  | And(e1,e2) -> And(trace1 e1, e2)
  (* or *)
  | Or(True,_) -> True  
  | Or(False,e2) -> e2
  | Or(e1,e2) -> Or(trace1 e1, e2)
  (* succ & pred *)
  | Pred(Succ(e)) when is_nv e -> e
  | Succ(e) -> Succ(trace1 e)
  | Pred(e) -> 
    let value = trace1 e in 
    if value <> Zero then Pred(trace1 e) else raise NoRuleApplies 
  (* iszero *)
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e-> False
  | IsZero(e) -> IsZero(trace1 e) 
  (* terminals *)
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let getNat = function
  Nat x -> x
 | _ -> raise NotNatInNatFun
;;

let getBool = function
  Bool x -> x
 | _ -> raise NotBoolInBoolFun
;;

(* TODO *)
let rec eval = function
    True -> Bool true
  | False -> Bool false
  | If(e0,e1,e2) -> 
    let condition = getBool(eval e0) in
    if condition then eval e1 else eval e2
  | Not e -> 
    let value = getBool(eval e) in
    if value then Bool false else Bool true
  | And (e1,e2)->
    let value1 = getBool(eval e1) in
    let value2 = getBool(eval e2) in
    Bool(value1 && value2)
  | Or (e1,e2)-> 
    let value1 = getBool(eval e1) in
    let value2 = getBool(eval e2) in
    Bool(value1 || value2)
  | Zero -> Nat 0
  | Succ(e) -> 
    let value = getNat(eval e) in
    Nat (value + 1)
  | Pred(e) -> 
    let value = getNat(eval e) in 
    if value>0 then Nat (value - 1 ) else raise PredBelowZero 
  | IsZero(e) -> if getNat(eval e) = 0 then Bool true else Bool false
;; 

let rec typecheck = function
    True  -> BoolT
  | False -> BoolT
  | Zero -> NatT
  | Pred(e) | Succ(e) -> 
    if (typecheck e) = NatT then NatT 
    else raise @@ TypeError((string_of_expr e)^" has type "^string_of_type(BoolT)^" but type "^string_of_type(NatT)^" was expected")
  | IsZero(e) -> 
    if (typecheck e) = NatT then BoolT 
    else raise @@ TypeError((string_of_expr e)^" has type "^string_of_type(BoolT)^" but type "^string_of_type(NatT)^" was expected")
  | Not(e) -> 
    if (typecheck e) = BoolT then BoolT 
    else raise @@ TypeError((string_of_expr e)^" has type "^string_of_type(NatT)^" but type "^string_of_type(BoolT)^" was expected")
  | Or(e1,e2) | And(e1, e2) ->
    if (typecheck e1) = BoolT then 
      if (typecheck e2) = BoolT then BoolT
      else raise @@ TypeError((string_of_expr e2)^" has type "^string_of_type(NatT)^" but type "^string_of_type(BoolT)^" was expected")
    else raise @@ TypeError((string_of_expr e1)^" has type "^string_of_type(NatT)^" but type "^string_of_type(BoolT)^" was expected")
  | If(e1,e2,e3) ->
    if (typecheck e1) = BoolT then 
      let type_e2 = (typecheck e2) in
      let type_e3 = (typecheck e3) in
      if type_e2 = type_e3 then type_e2
      else raise @@ TypeError((string_of_expr e3)^" has type "^string_of_type(type_e3)^" but type "^string_of_type(type_e2)^" was expected")
    else raise @@ TypeError((string_of_expr e1)^" has type "^string_of_type(NatT)^" but type "^string_of_type(BoolT)^" was expected")
;;