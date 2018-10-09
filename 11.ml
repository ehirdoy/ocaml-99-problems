(* 11. Modified run-length encoding. (easy)

Modify the result of the previous problem in such a way that
if an element has no duplicates it is simply copied into the result list.
Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous,
one needs to define a type to hold both single elements and sub-lists.
*)

#use "9.ml"

type 'a rle = One of 'a | Many of int * 'a

let encode lst =
  let f l =
    let len = List.length l in
    if len = 1 then One (List.hd l) else Many (List.length l, List.hd l)
  in
  List.map f (pack lst)

let test =
  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")]
