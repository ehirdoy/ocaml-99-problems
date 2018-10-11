(* 19. Rotate a list N places to the left. (medium) *)

let split lst nr =
  let rec aux n acc = function
    | [] -> (List.rev acc, [])
    | x::xs as l ->
      if n = 0 then (List.rev acc, l) else aux (n-1) (x::acc) xs
  in
  let update n =
    if n >= 0 then n else n + (List.length lst) in

  aux (update nr) [] lst

let rotate lst nr = match lst with
  | [] -> []
  | [x] -> [x]
  | x::xs ->
    let (h, t) = split lst nr in
    t @ h

let test = rotate [] 3 = []
let test = rotate ["a"] 1 = ["a"]
let test = rotate ["a";"b"] 1 = ["b";"a"]
let test = rotate ["a";"b"] 2 = ["a";"b"]
let test =
  rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
       = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
let test =
  rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)
       = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
