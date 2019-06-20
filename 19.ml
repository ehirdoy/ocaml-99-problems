(* 19. Rotate a list N places to the left. (medium) *)

let rec rotate list nr = match list, nr with
  | [], _ -> []
  | list, nr when nr=0 -> list
  | list, nr when nr<0 -> rotate list (List.length list + nr)
  | x::xs, nr (*when nr>0*) -> rotate (xs @ [x]) (nr-1)


let test01 = rotate [] 3 = []
let test02 = rotate ["a"] 1 = ["a"]
let test03 = rotate ["a";"b"] 1 = ["b";"a"]
let test04 = rotate ["a";"b"] 2 = ["a";"b"]
let test05 =
  rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
       = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
let test06 =
  rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)
       = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
