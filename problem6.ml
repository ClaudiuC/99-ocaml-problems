(* Find out whether a list is a palindrom *)

open Core.Std

let is_palindrome l = l = Problem5.rev l ;;

assert (is_palindrome ["a";"n";"i";"n";"a"] = true)
assert (not (is_palindrome ["foo"; "bar"]) = true)
