(* 12. Decode a run-length encoded list. (medium)

Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.
*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode lst =
  let rec rep n x base =
    if n = 1 then x::base else rep (n-1) x (x::base)
  in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> match x with
      | One x -> aux (x::acc) xs
      | Many (n, x) -> aux (rep n x acc) xs
  in
  List.rev (aux [] lst)

let test =
  decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]
  = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
