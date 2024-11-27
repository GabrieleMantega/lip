open BoolexprLib.Main
open BoolexprLib.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = parse "false" |> eval = false
let%test "test_eval_2" = parse "true" |> eval = true
let%test "test_eval_3" = parse "if true then false else true" |> eval = false
let%test "test_eval_4" = parse "if false then false else true" |> eval = true
let%test "test_eval_5" = parse "if true then (if true then false else true) else (if true then true else false)" |> eval = false
let%test "test_eval_6" = parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" |> eval = false
let%test "test_eval_7" = parse "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" |> eval = false


(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = parse "if true then true else true" |> trace1 = True
let%test "test_trace1_2" = parse "if true then (if true then false else true) else true" |> trace1 = If(True,False,True)
let%test "test_trace1_3" = parse "if(if(if(true)then false else true) then true else true)then false else false" |> trace1 = If(If(False,True,True),False,False)
let%test "test_trace1_4" = let v = (parse "true") in try (is_value(trace1 v)) with NoRuleApplies -> is_value(v)

let%test "test_trace" = parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" |> trace |> List.length  < 10 + 1 (* counts base case *) 