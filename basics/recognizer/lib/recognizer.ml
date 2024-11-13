let binseq l = List.for_all (fun x -> x='0' || x='1') l;;

let lang1 l =
  (List.length l)>=1 && binseq l;;

let lang2 = function
    [] -> true
  | x::l -> (x='0' || x='1') && (List.for_all (fun x -> x='1') l);;

let rec lang3_tl l = match l with
    [] -> false
  | [x] -> x='0'
  | x::tl -> (x='0' || x='1') && lang3_tl tl
;;

let lang3 l = 
  l <> [] &&
  (List.hd l)='0' && 
  lang3_tl (List.tl l)
;;  

let lang4 l = 
  binseq l && (List.fold_left (fun ones x -> if x='1' then ones+1 else ones) 0 l)=2
;;

let rec lang5 = function
    x::y::l when l == [] -> (x='0' || x='1') && x=y
  | x::y::l when l <> [] -> (x='0' || x='1') && x=y && lang5 l 
  | _ -> false
;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
