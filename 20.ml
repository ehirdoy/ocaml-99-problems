(* 20. Remove the K'th element from a list. (easy)
 *
 * The first element of the list is numbered 0, the second 1,... *)

let rec remove_at nth list = match nth, list with
  | nth, [] -> []
  | 0, x::xs -> xs
  | nth, x::xs -> x :: remove_at (nth - 1) xs

let test = remove_at 1 [] = []
let test = remove_at 0 ["a"] = []
let test = remove_at 1 ["a";"b";"c";"d"]
           = ["a"; "c"; "d"]
