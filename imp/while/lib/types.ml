open Ast

type exprval = Bool of bool | Nat of int        (* value of an expression *)
type state = ide -> exprval                     (* state = map from identifiers to expression values *)
type conf = St of state | Cmd of cmd * state    (* configuration = state | (command,state) *)

exception TypeError of string
exception UnboundVar of string
exception NoRuleApplies
