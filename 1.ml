(*
 1. Write a function last :
 'a list -> 'a option that returns the last element of a list. (easy)
*)

let rec last = function
  | [] -> None
  | x :: [] -> Some x
  | x::xs -> last xs

let test = last [] = None
let test = last [1] = Some (1)
let test = last [2;1] = Some (1)
let test = last [3;1;2] = Some (2)


