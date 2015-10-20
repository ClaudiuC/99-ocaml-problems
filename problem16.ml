(* Drop every N'th element from a list *)

let drop l count =
  let rec aux n = function
    | [] -> []
    | hd :: tl -> if n = count
      then aux 1 tl
      else hd :: aux (n+1) tl
  in
  aux 1 l
;;

assert (drop ["a";"b";"c";"d"] 2 = ["a";"c"])

