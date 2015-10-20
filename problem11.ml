(* Modified run-length encoding *)

type 'a rle = 
  | One of 'a
  | Many of int * 'a
;;

let get_rle count elem = 
  if count = 1 then One elem
  else Many (count, elem)
;;

let encode l = 
  let rec aux count acc = function
    | [] -> []
    | [x] -> (get_rle (count+1) x) :: acc 
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (count+1) acc tl
    | hd :: tl -> aux 0 ((get_rle (count+1) hd) :: acc) tl 
  in
  Problem5.rev (aux 0 [] l)
;;

let input = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] ;;
let expected = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
Many (4, "e")] ;;
assert (encode input = expected) 
