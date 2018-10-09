(*
	2. Find the last but one (last and penultimate) elements of a list. (easy)
*)

let rec last_two = function
  | [] -> None | [_] -> None
  | [x;y] -> Some (x, y)
  | _ :: xs -> last_two xs

let test = last_two [] = None
let test = last_two ["a"] = None
let test = last_two ["a";"b";"c";"d"] = Some ("c", "d")
