(*
        6. Find out whether a list is a palindrome. (easy)
        HINT: a palindrome is its own reverse.
*)

#use "5.ml"

let is_palindrome lst =
  lst = (rev lst)

(*
# is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
- : bool = true
# not (is_palindrome [ "a" ; "b" ]);;
- : bool = true
*)
let test = is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true
let test = not (is_palindrome [ "a" ; "b" ]) = true
