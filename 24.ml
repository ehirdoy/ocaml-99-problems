(* 24. Lotto: Draw N different random numbers from the set 1..M. (easy)
 *
 * The selected numbers shall be returned in a list. *)

let lotto_select n max =
  let rec aux acc = function
    | 0 -> acc
    | n ->
      let x = Random.int (max+1) in
      if List.mem x acc then aux acc n else
        aux (x::acc) (n-1)
  in
  aux [] n |> List.sort (fun a b -> a - b)

let test = lotto_select 6 49
let test = lotto_select 0 49 = []
