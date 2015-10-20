(* Eliminate consecutive duplicates of list elements *)

let rec compress = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) when hd = hd' -> compress tl 
  | hd :: tl -> hd :: compress tl
;;

let ex = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] ;;

assert (compress ex = ["a"; "b"; "c"; "a"; "d"; "e"])
