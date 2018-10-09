(* 7. Flatten a nested list structure. (medium) *)

(* There is no nested list type in OCaml, so we need to define one
   first. A node of a nested list is either an element, or a list of
   nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;
type 'a node = One of 'a | Many of 'a node list

let rec flatten = function
  | [] -> []
  | [One x] -> [x]
  | One x :: xs -> x :: flatten xs
  | Many xs :: ys -> flatten xs @ flatten ys

let test =
  flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] =
  ["a"; "b"; "c"; "d"; "e"]
let test = flatten [] = []
let test = flatten [ One "a"] = ["a"]
let test = flatten [ Many [One "a"]] = ["a"]
let test = flatten [ One "a";One "b"] = ["a";"b"]
let test = flatten [ Many [ One "a";One "b"]] = ["a";"b"]
let test = flatten [ One "a";One "b";One "c"] = ["a";"b";"c"]
