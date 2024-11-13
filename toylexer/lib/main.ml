open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl


let uniq l = List.rev (List.fold_left (fun l' x -> if List.for_all (fun y -> y<>x) l' then x::l' else l') [] l);;

let rec take_n n = function
  [] -> []
| x::l when n>0 -> x::(take_n (n-1) l)
| _ -> []
;;

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n tokens =
  let count = List.map 
    (fun x -> (x, List.fold_left (fun acc y -> if x=y then acc+1 else acc) 0 tokens)) 
    (uniq tokens) 
  in
  let sorted_count = List.sort 
    (fun (_, x) (_, y) -> if x>y then -1 else if x=y then 0 else 1) 
    count 
  in
  take_n n sorted_count
;;