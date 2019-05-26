(* 10. Run-length encoding of a list. (easy) *)

#use "9.ml"

let encode lst =
  let open List in
  map (fun l -> (length l, hd l)) (pack lst)

let test = encode [] = []
let test = encode ["a"] = [(1, "a")]
let test =
  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

