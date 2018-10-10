(* 14. Duplicate the elements of a list. (easy) *)

let duplicate lst =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (x::x::acc) xs
  in
  List.rev (aux [] lst)

let test = duplicate [] = []
let test = duplicate ["a"] = ["a";"a"]
let test = duplicate ["a";"b"] = ["a";"a";"b";"b"]
let test = duplicate ["a";"b";"c";"c";"d"]
           = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
