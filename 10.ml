(* 10. Run-length encoding of a list. (easy) *)

let encode lst =
  let rec aux num acc src = match src with
    | [] -> []
    | [x] -> (num+1, x)::acc
    | x::(x'::_ as t) ->
      if x = x' then aux (num+1) acc t
      else aux 0 ((num+1,  x)::acc) t
  in
  List.rev (aux 0 [] lst)

let test = encode [] = []
let test = encode ["a"] = [(1, "a")]
let test =
  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

