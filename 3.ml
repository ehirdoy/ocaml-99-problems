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

let rec nth n = function
  | [] -> None
  | x :: xs ->
    if n = 0 then Some x
    else if n > 0 then nth (n - 1) xs
    else None

let test = nth 2 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c"
let test = nth 2 [ "a" ] = None
