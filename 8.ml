(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress = function
  | x::(x'::_ as t) -> if x = x' then compress t else x::compress t
  | smaller -> smaller

let test =
  compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  ["a"; "b"; "c"; "a"; "d"; "e"]
