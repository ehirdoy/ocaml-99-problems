(*
        5. Reverse a list. (easy)
        OCaml standard library has List.rev but we ask that you reimplement it.
*)

let rev lst =
  let rec __rev xs ys = match xs with
    | [] -> ys
    | x' :: xs' -> __rev xs' (x'::ys)
  in
  __rev lst []

(*
# rev ["a" ; "b" ; "c"];;
- : string list = ["c"; "b"; "a"]
*)
let test = rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"]
