(* Insert an element at a given position into a list *)

let rec insert_at elem n = function
  | [] -> [elem]
  | hd :: tl as l -> if n = 0 
    then elem :: l
    else hd :: insert_at elem (n-1) tl
;;

assert (insert_at "bar" 1 ["foo";"baz"] = ["foo";"bar";"baz"])
