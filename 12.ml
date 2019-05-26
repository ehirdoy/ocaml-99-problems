(* 12. Decode a run-length encoded list. (medium)

Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.
*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode lst =
  List.fold_right (fun el acc ->
      match el with
      | One x -> x::acc
      | Many (n, ch) ->
        let rec aux acc = function
          | 0 -> acc
          | n -> aux (ch::acc) (n-1)
        in
        aux acc n
    ) lst []

let test =
  decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]
  = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
