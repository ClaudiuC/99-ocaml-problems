(* Duplicate the elements of a list *)

let rec duplicate = function
  | [] as l -> l
  | hd :: tl -> hd :: hd :: duplicate tl
;;

assert (duplicate ["a";"b";"c"] = ["a";"a";"b";"b";"c";"c"])
