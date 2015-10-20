(* Run-length encoding of a list *)


let encode l = 
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count+1, x) :: acc 
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (count+1) acc tl
    | hd :: tl -> aux 0 ((count+1, hd) :: acc) tl 
  in
  Problem5.rev (aux 0 [] l)
;;

let input = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] ;;
let expected = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] ;;
assert (encode input = expected) 
