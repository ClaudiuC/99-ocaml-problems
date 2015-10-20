(* Pack consecutive duplicates of list elements into sublists *)

let pack l = 
  let rec aux consecutive acc = function
    | [] -> []
    | [x] -> (x :: consecutive) :: acc 
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux (hd :: consecutive) acc tl
    | hd :: tl -> aux [] ((hd :: consecutive) :: acc) tl 
  in
  Problem5.rev (aux [] [] l)
;;

let ex = [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`d;`e;`e;`e;`e] ;;
assert (pack ex = [[`a;`a;`a;`a];[`b];[`c;`c];[`a;`a];[`d;`d];[`e;`e;`e;`e]]) ;;
