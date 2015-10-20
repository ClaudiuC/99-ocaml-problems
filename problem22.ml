(* Create a list containing all integers within a given range *)

let range x y =
  let rec aux x y = 
    if x > y then [] else x :: aux (x+1) y in
  if x > y then Problem5.rev (aux y x) else aux x y 
;;

assert(range 4 9 = [4; 5; 6; 7; 8; 9])
assert(range 9 4 = [9; 8; 7; 6; 5; 4])
