(* 15. Replicate the elements of a list a given number of times. (medium) *)

let replicate lst num =
  let rec prepend n acc x =
    if n = 0 then acc else prepend (n - 1) (x::acc) x in
  List.fold_left (prepend num) [] (List.rev lst)

let test = replicate [] 3 = []
let test = replicate ["a"] 2 = ["a";"a"]
let test = replicate ["a";"b";"c"] 3
           = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
