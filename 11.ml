(* 11. Modified run-length encoding. (easy)

Modify the result of the previous problem in such a way that
if an element has no duplicates it is simply copied into the result list.
Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous,
one needs to define a type to hold both single elements and sub-lists.
*)

type 'a rle = One of 'a | Many of int * 'a

let encode lst =
  let to_tuple n el =
    if n = 1 then One el else Many (n, el) in
  let rec aux num acc src = match src with
    | [] -> []
    | [x] -> (to_tuple (num+1) x)::acc
    | x::(x'::_ as t) ->
      if x = x' then aux (num+1) acc t
      else aux 0 ((to_tuple (num+1) x)::acc) t
  in
  List.rev (aux 0 [] lst)

let test = encode [] = []
let test = encode ["a"] = [One "a"]
let test =
  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")]
