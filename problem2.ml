(* Find the last but one (last and penultimate) elements of a list. *)

open Core.Std

let rec last_two = function
  | [] | [_] -> None
  | [x;y] -> Some (x, y)
  | _ :: tl -> last_two tl
;;

assert(last_two [1;2;3;4] = Some (3,4))
assert(last_two [1;2] = Some (1,2))
assert(last_two [1] = None)
assert(last_two [] = None)

