(* Find the k'th element of a list *)

let rec kth k = function 
  | [] -> None
  | hd :: tl -> if k = 1 then Some hd else kth (k-1) tl
;;

assert(kth 3 [1;2;3] = Some 3)
assert(kth 1 [1;2;3] = Some 1)
assert(kth 0 [1;2;3] = None)
assert(kth 4 [1;2;3] = None)
assert(kth 99 [] = None)
