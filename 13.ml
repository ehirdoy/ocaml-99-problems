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

let encode lst =
  let rec aux count acc = function
    | [] -> acc
    | [x] -> (count+1, x)::acc
    | x::(x'::_ as t) ->
      if x = x' then aux (count+1) acc t
      else aux 0 ((count+1, x)::acc) t
  in
  aux 0 [] lst
  |> List.rev
  |> List.map (fun (n, x) -> if n = 1 then One x else Many (n, x))

let test = encode [] = []
let test = encode ["a"] = [One "a"]
let test = encode ["a";"a"] = [Many (2, "a")]
let test = encode ["a";"a";"b"] = [Many (2, "a"); One "b"]
let test = encode ["b";"a";"a"] = [One "b"; Many (2, "a")]
let test =
  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
