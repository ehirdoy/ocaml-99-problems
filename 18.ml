(* 18. Extract a slice from a list. (medium)
 *
 * Given two indices, i and k, the slice is the list containing the elements
 * between the i'th and k'th element of the original list (both limits included).
 * Start counting the elements with 0
 * (this is the way the List module numbers elements). *)

let slice lst i k =
  let rec skip n acc = function
    | [] -> acc
    | x::xs ->
      if n = 0 then acc
      else
        let acc = xs in
        skip (n-1) acc xs
  in
  let rec take n acc = function
    | [] -> acc
    | x::xs ->
      if n = 0 then acc
      else
        let acc = acc @ [x] in
        take (n-1) acc xs
  in
  skip i [] lst |> take (k-i+1) []

let test = slice [] 2 6 = []
let test = slice ["a"] 2 6 = []
let test = slice ["a";"b";"c"] 2 6 = ["c"]

                (* 0   1  |2   3   4   5   6|  7   8   9 *)
let test = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
           = ["c"; "d"; "e"; "f"; "g"]
