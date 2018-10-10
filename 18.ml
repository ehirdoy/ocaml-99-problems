(* 18. Extract a slice from a list. (medium)
 *
 * Given two indices, i and k, the slice is the list containing the elements
 * between the i'th and k'th element of the original list (both limits included).
 * Start counting the elements with 0
 * (this is the way the List module numbers elements). *)


let slice lst i k =
  let rec aux acc n = function
    | [] -> acc
    | x::xs ->
      if n > k then acc
      else if n >= i then aux (x::acc) (n+1) xs
      else aux acc (n+1) xs
  in
  aux [] 0 lst |> List.rev

let test = slice [] 2 6 = []
let test = slice ["a"] 2 6 = []
let test = slice ["a";"b";"c"] 2 6 = ["c"]

                (* 0   1  |2   3   4   5   6|  7   8   9 *)
let test = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
           = ["c"; "d"; "e"; "f"; "g"]
