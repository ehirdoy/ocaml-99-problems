(*
    13. Run-length encoding of a list (direct solution). (medium)

    Implement the so-called run-length encoding data compression method directly.
    I.e. don't explicitly create the sublists containing the duplicates,
    as in problem "Pack consecutive duplicates of list elements into sublists",
    but only count them.

    As in problem "Modified run-length encoding",
    simplify the result list by replacing the singleton lists (1 X) by X.
*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let rle x = function
  | 0 -> One x
  | n -> Many (n+1, x)

let test02 = rle "a" 0 = One "a"
let test03 = rle "a" 2 = Many (3, "a")

let encode lst =
  let rec aux count acc = function
    | [] -> acc
    | [x] -> (rle x count)::acc
    | x::(x'::_ as t) ->
      if x = x' then aux (count+1) acc t
      else aux 0 ((rle x count)::acc) t
  in
  aux 0 [] lst
  |> List.rev

let test11 = encode [] = []
let test12 = encode ["a"] = [One "a"]
let test13 = encode ["a";"a"] = [Many (2, "a")]
let test14 = encode ["a";"a";"b"] = [Many (2, "a"); One "b"]
let test15 = encode ["b";"a";"a"] = [One "b"; Many (2, "a")]
let test16 =
  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
