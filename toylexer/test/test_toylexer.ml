open Toylexer.Token
open Toylexer.Main

let%test "test_frequencies_1" =
  lexer "x=1; y=x+1" |> frequency 3 = [(ID "x", 2); (ASSIGN, 2); (CONST "1", 2)];;
let%test "test_frequencies_2" =
  lexer "x=3; x=x+1; y=(x-(3-2))" |> frequency 4 = [(ID "x", 4); (ASSIGN, 3); (CONST "3", 2); (SEQ, 2)];;
let%test "test_frequencies_3" =
  lexer "x=1; y=(1+(1+(1+2)))" |> frequency 3 = [(CONST "1", 4); (LPAREN, 3); (PLUS, 3)];;
let%test "test_frequencies_4" =
  lexer "x=======2;;; x x x x" |> frequency 2 = [(ASSIGN, 7); (ID "x", 5)];; (* correct! lexers don't check semantics *)
