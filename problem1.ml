(* Write a function last : 'a list -> 'a option that returns the last element of a list. *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl
;;

assert (last [1;2;3;4] = Some 4)
assert (last [] = None)
