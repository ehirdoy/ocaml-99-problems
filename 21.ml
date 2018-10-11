(* 21. Insert an element at a given position into a list. (easy)
 *
 * Start counting list elements with 0.
 * If the position is larger or equal to the length of the list,
 * insert the element at the end.
 * (The behavior is unspecified if the position is negative.) *)

let rec insert_at s nth = function
  | [] -> [s]
  | x::xs as l ->
    if nth = 0 then s::l else x :: insert_at s (nth - 1) xs

let test = insert_at "xxxxx" 0 [] = ["xxxxx"]
let test = insert_at "xxxxx" 3 [] = ["xxxxx"]
let test = insert_at "alfa" 1 ["a";"b";"c";"d"]
           = ["a"; "alfa"; "b"; "c"; "d"]
let test = insert_at "alfa" 3 ["a";"b";"c";"d"]
           = ["a"; "b"; "c"; "alfa"; "d"]
let test = insert_at "alfa" 4 ["a";"b";"c";"d"]
           = ["a"; "b"; "c"; "d"; "alfa"]
