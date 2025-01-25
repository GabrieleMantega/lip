open Ast
(* open Types  *)

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

(* 
let getInt = function
  Int x -> x
 | _ -> raise @@ TypeError("Int type expected")
;;

let getBool = function
  Bool x -> x
 | _ -> raise @@ TypeError("Bool type expected")
;;

let rec findLoc st x = 
  let env = (topenv st) in
  try (env x)
  with _ -> findLoc (make_state (popenv st) (getmem st) (getloc st)) x
;;

(* ALTERNATIVE IMPLEMENTATION FOR "findLoc"
 *
 *  let rec findLoc (envlist:env list) (x:ide) = 
 *    match envlist with
 *      [] -> failwith "empty environment stack"
 *    | env::l -> try (env x) with _ -> (findLoc l x)
 *  ;;
 *)

let loc_of_envval = function
  BVar l | IVar l -> l
;;

let rec eval_expr (st:state) = function
    True -> Bool true
  | False -> Bool false
  | Const(n) -> Int n
  | Not(e) -> Bool(not @@ getBool @@ eval_expr st e)  
  | And(e1,e2) -> Bool((getBool @@ eval_expr st e1) && (getBool @@ eval_expr st e2))
  | Or(e1,e2) -> Bool((getBool @@ eval_expr st e1) || (getBool @@ eval_expr st e2))
  | Add(e1,e2) -> Int((getInt @@ eval_expr st e1) + (getInt @@ eval_expr st e2))
  | Sub(e1,e2) -> Int((getInt @@ eval_expr st e1) - (getInt @@ eval_expr st e2))
  | Mul(e1,e2) -> Int((getInt @@ eval_expr st e1) * (getInt @@ eval_expr st e2))
  | Eq(e1,e2) -> Bool(eval_expr st e1 = eval_expr st e2)
  | Leq(e1,e2) -> Bool((getInt @@ eval_expr st e1) <= (getInt @@ eval_expr st e2))
  | Var(x) -> (getmem st) @@ loc_of_envval @@ findLoc st x
;;

let eval_decl st d = 
  let loc' = getloc st in
  let st' = setloc st (loc'+1) in
  let env' =
    (match d with
        IntVar x -> bind_env (topenv st') (x) (IVar loc')
      | BoolVar x -> bind_env (topenv st') (x) (BVar loc'))
  in
  setenv (st') (env'::popenv st')
;;

let rec eval_decl_list st = function
    [] -> st
  | d::l -> eval_decl_list (eval_decl (st) (d)) l 
;;

let loc_typecheck v l =
  let vtype = match v with Bool _ -> BoolT | Int _ -> IntT in
  let ltype = match l with BVar _ -> BoolT | IVar _ -> IntT in
  if vtype <> ltype then 
    raise @@ TypeError ("Cannot assign "^string_of_type(vtype)^" value to "^string_of_type(ltype)^" variable")
  else None
;;

let rec trace1 = function
  Cmd(cmd,st) ->
    (match cmd with
      Skip -> St st
    | Assign(x,v) -> 
      let value = eval_expr st v in
      let loc = findLoc st x in
      let _ = loc_typecheck value loc in
      let st' = 
        setmem st (
          bind_mem 
            (getmem st)
            (loc_of_envval @@ loc)   
            value)
      in St st'
    | Seq(c1,c2) -> 
      (let res = trace1 @@ Cmd(c1,st) in
        match res with
          St st' -> Cmd(c2,st')
        | Cmd(c1',st') -> Cmd(Seq(c1',c2),st'))
    | If(e,c1,c2) -> if (eval_expr st e = Bool true) then Cmd(c1,st) else Cmd(c2,st)
    | While(e,c) -> if (eval_expr st e = Bool true) then Cmd(Seq(c,While(e,c)), st) else St st
    | Decl(dl,c) -> 
      let st' = setenv (st) (bottom_env::getenv st) in
      let st'' = eval_decl_list (st') dl in Cmd(Block(c),st'')
    | Block(c) -> 
      match trace1 @@ Cmd(c, st) with
        St st' -> St (setenv st' (popenv st'))
      | Cmd(c',st') -> Cmd(Block c',st'))
  | St _ -> raise NoRuleApplies
;;

let trace n cmd =
  let rec rectrace n conf =
    if n>=0 then
      try 
        let step = trace1 conf in
        conf::(rectrace (n-1) step) 
      with NoRuleApplies -> [conf]
    else []
  in

  rectrace n (Cmd(cmd,state0))
;; *)