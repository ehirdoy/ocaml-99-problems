(*
        6. Find out whether a list is a palindrome. (easy)
        HINT: a palindrome is its own reverse.
*)

#use "5.ml"

exception Fatal

let is_palindrome lst =
  let rec is_equal xs ys = match xs with
    | [] -> true
    | x'::xs' -> match ys with
      | [] -> raise Fatal
      | y'::ys' -> if x' = y' then is_equal xs' ys' else false
  in
  is_equal lst (rev lst)

(*
# is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
- : bool = true
# not (is_palindrome [ "a" ; "b" ]);;
- : bool = true
*)
let test = is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true
let test = not (is_palindrome [ "a" ; "b" ]) = true
