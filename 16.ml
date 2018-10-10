(* 16. Drop every N'th element from a list. (medium) *)

let drop lst num =
  let f (idx, acc) el =
    if idx = num then (1, acc) else (idx+1, el::acc) in
  match List.fold_left f (1, []) lst with
  | _, acc -> List.rev acc

let test1 = drop [] 3 = []
let test2 = drop ["a"] 3 = ["a"]
let test3 = drop ["a";"b";"c"] 3 = ["a";"b"]
let test = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
           = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
