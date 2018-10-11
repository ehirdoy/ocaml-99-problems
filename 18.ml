(* 18. Extract a slice from a list. (medium)
 *
 * Given two indices, i and k, the slice is the list containing the elements
 * between the i'th and k'th element of the original list (both limits included).
 * Start counting the elements with 0
 * (this is the way the List module numbers elements). *)

let slice lst i k =
  let rec fold_until ~f n acc = function
    | [] -> acc
    | x::xs as l-> if n = 0 then acc else fold_until ~f (n-1) (f l acc) xs
  in
  let skip n acc l = fold_until ~f:(fun l a -> List.tl l) n acc l in
  let take n acc l = fold_until ~f:(fun l a -> a @ [List.hd l]) n acc l in

  skip i [] lst |> take (k-i+1) []

let test = slice [] 2 6 = []
let test = slice ["a"] 2 6 = []
let test = slice ["a";"b";"c"] 2 6 = ["c"]

                (* 0   1  |2   3   4   5   6|  7   8   9 *)
let test = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
           = ["c"; "d"; "e"; "f"; "g"]
