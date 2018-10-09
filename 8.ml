(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress = function
  | [] -> []
  | x::[] -> [x]
  | x::x'::xs -> if x = x' then compress (x::xs) else x::compress (x'::xs)

let test =
  compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  ["a"; "b"; "c"; "a"; "d"; "e"]
