(* Reverse a list *)

let rev l = 
  let rec aux acc = function 
    | [] -> acc
    | hd::tl -> aux (hd::acc) tl in
  aux [] l
;; 

assert (rev [1;2;3;4] = [4;3;2;1])
assert (rev [] = [])
assert (rev [1] = [1])
