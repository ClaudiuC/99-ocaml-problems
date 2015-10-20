(* Find the number of elements of a list *)

let rec length = function
  | [] -> 0
  | _ :: tl -> 1 + length tl
;;

let rec length_tail_recursive l n = 
  match l with
    | [] -> n
    | _ :: tl -> length_tail_recursive tl (n+1)
;;

let length_better l = length_tail_recursive l 0 ;;

assert(length [1;2;3] = 3)
assert(length [] = 0)
assert(length_better [1;2;3] = 3)
assert(length_better [] = 0)
