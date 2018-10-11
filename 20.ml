(* 20. Remove the K'th element from a list. (easy)
 *
 * The first element of the list is numbered 0, the second 1,... *)

let remove_at nth lst =
  let rec aux n acc l = match l with
    | [] -> List.rev acc @ l
    | x::xs ->
      if n = 0 then (List.rev acc) @ xs else aux (n-1) (x::acc) xs
  in
  aux nth [] lst

let test = remove_at 1 [] = []
let test = remove_at 0 ["a"] = []
let test = remove_at 1 ["a";"b";"c";"d"]
           = ["a"; "c"; "d"]
