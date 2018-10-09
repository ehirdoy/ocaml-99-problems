(*
        4. Find the number of elements of a list. (easy)

        OCaml standard library has List.length but we ask that you reimplement it.
        Bonus for a tail recursive solution.
*)

let length lst =
  let rec aux n = function
    | [] -> n
    | x :: xs -> aux (n+1) xs
  in aux 0 lst

let test = length [ "a" ; "b" ; "c"] = 3
let test = length [] = 0
