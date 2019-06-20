(* 17. Split a list into two parts; the length of the first part is given. (easy)

If the length of the first part is longer than the entire list,
then the first part is the list and the second part is empty.

*)

let split list number =
  let rec aux first = function
    | [] -> (List.rev first, [])
    | x::xs when List.length first = number -> (List.rev first, x::xs)
    | x::xs when List.length first < number -> aux (x::first) xs
    | _ (*when List.length first > number*) -> assert false
  in
  aux [] list

let test1 = split [] 3 = ([], [])
let test2 = split ["a"] 3 = (["a"], [])
let test3 = split ["a";"b"] 0 = ([], ["a";"b"])
let test4 = split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
            = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
let test5 = split ["a";"b";"c";"d"] 5
            = (["a"; "b"; "c"; "d"], [])

