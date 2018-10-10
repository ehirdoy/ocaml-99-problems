(* 15. Replicate the elements of a list a given number of times. (medium) *)

let replicate lst num =
  let rec aux count acc = function
    | [] -> acc
    | x::xs ->
      if count = 1 then aux num (x::acc) xs else aux (count-1) (x::acc) (x::xs)
  in
  List.rev (aux num [] lst)

let test = replicate [] 3 = []
let test = replicate ["a"] 2 = ["a";"a"]
let test = replicate ["a";"b";"c"] 3
           = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
