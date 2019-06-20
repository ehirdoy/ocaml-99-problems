(* 16. Drop every N'th element from a list. (medium) *)

let drop list number =
  let rec aux counter = function
    | [] -> []
    | x::xs -> match counter with
      | 1 -> aux number xs
      | _ -> x :: aux (counter-1) xs
  in
  aux number list

let test1 = drop [] 3 = []
let test2 = drop ["a"] 3 = ["a"]
let test3 = drop ["a";"b";"c"] 3 = ["a";"b"]
let test = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
           = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
