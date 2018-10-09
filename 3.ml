(* 3. Find the k'th element of a list. (easy)  *)

(*
# at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
- : string option = Some "c"
# at 3 [ "a" ];;
- : string option = None

REMARK: OCaml has List.nth which numbers elements from 0 and raises an exception if the index is out of bounds.

# List.nth [ "a" ; "b"; "c"; "d"; "e" ] 2;;
- : string = "c"
# List.nth [ "a" ] 2;;
Exception: Failure "nth".
*)

exception End_of_index

let rec nth lst n =
  match lst with
  | [] -> raise End_of_index
  | x :: xs ->
    if n = 0 then x
    else if n > 0 then nth xs (n - 1)
    else raise End_of_index

let test = nth [ "a" ; "b"; "c"; "d"; "e" ] 2 = "c"
let test = nth [ "a" ] 2
