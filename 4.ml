(*
        4. Find the number of elements of a list. (easy)

        OCaml standard library has List.length but we ask that you reimplement it.
        Bonus for a tail recursive solution.
*)

let rec length = function
  | [] -> 0
  | x :: xs -> 1 + length xs

let test = length [ "a" ; "b" ; "c"] = 3
let test = length [] = 0

