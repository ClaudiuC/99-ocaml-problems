(* Flatten a nested list structure *)

type 'a node = 
  | One of 'a
  | Many of 'a node list
;;

let flatten l = 
  let rec aux acc = function
    | [] -> acc
    | One hd :: tl -> aux (hd :: acc) tl
    | Many hd :: tl -> aux (aux acc hd) tl in
  List.rev (aux [] l)
;;

let ex = 
  [One `a; Many [ One `b; Many [ One `c; One `d]; One `e]]
;;

assert (flatten [] = [])
assert (flatten ex = [`a;`b;`c;`d;`e])
