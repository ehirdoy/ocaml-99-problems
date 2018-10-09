(*
        5. Reverse a list. (easy)
        OCaml standard library has List.rev but we ask that you reimplement it.
*)

let rev lst =
  let rec aux xs acc = match xs with
    | [] -> acc
    | x' :: xs' -> aux xs' (x'::acc)
  in
  aux lst []

(*
# rev ["a" ; "b" ; "c"];;
- : string list = ["c"; "b"; "a"]
*)
let test = rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"]
