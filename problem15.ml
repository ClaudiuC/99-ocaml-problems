(* Replicate the elements of a list a given number of times *)

let replicate l n = 
  let rec prepend acc str n = 
    if n > 0 then prepend (str :: acc) str (n-1) else acc
  in
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (prepend acc hd n) tl 
  in
  aux [] (Problem5.rev l)
;;

assert (replicate ["a";"b"] 3 = ["a";"a";"a";"b";"b";"b"])
