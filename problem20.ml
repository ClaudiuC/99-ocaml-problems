(* Remove the K'th element from a list *)

let rec remove_at n = function
  | [] -> []
  | hd :: tl -> if n = 0 
    then tl 
    else hd :: remove_at (n-1) tl 
;;

assert (remove_at 1 ["a";"b";"c"] = ["a";"c"])
