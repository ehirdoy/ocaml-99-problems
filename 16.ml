(* 16. Drop every N'th element from a list. (medium) *)

let drop lst num =
  let rec aux i = function
    | [] -> []
    | x::xs -> if i = num then aux 1 xs else x :: aux (i+1) xs
  in
  aux 1 lst

let test1 = drop [] 3 = []
let test2 = drop ["a"] 3 = ["a"]
let test3 = drop ["a";"b";"c"] 3 = ["a";"b"]
let test = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
           = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
