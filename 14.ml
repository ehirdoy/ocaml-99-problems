(* 14. Duplicate the elements of a list. (easy) *)

let rec duplicate = function
  | [] -> []
  | x::xs ->  x::x::(duplicate xs)

let test = duplicate [] = []
let test = duplicate ["a"] = ["a";"a"]
let test = duplicate ["a";"b"] = ["a";"a";"b";"b"]
let test = duplicate ["a";"b";"c";"c";"d"]
           = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
