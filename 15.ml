(* 15. Replicate the elements of a list a given number of times. (medium) *)

let replicate list n =
  let rec rep n x acc = match n with
    | 0 -> acc
    | n -> rep (n-1) x (x::acc)
  in
  List.fold_right (rep n) list []

let test = replicate [] 3 = []
let test = replicate ["a"] 2 = ["a";"a"]
let test = replicate ["a";"b";"c"] 3
           = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
