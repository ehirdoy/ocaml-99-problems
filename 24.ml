(* 24. Lotto: Draw N different random numbers from the set 1..M. (easy)
 *
 * The selected numbers shall be returned in a list. *)

exception Shouldnt_happen

let random_intlist n max =
  let n = if n > max then max else n in
  let rec aux n acc max =
    if n < 0 then raise Shouldnt_happen
    else if n = 0 then acc
    else
      let num = Random.int max in
      let rec is_overlap nr = function
        | [] -> false
        | x::xs -> if nr = x then true else is_overlap nr xs in
      if is_overlap num acc then aux n acc max
      else aux (n-1) (num::acc) max in
  aux n [] max |> List.sort (fun x y -> x - y)

let lotto_select n max = random_intlist n (max+1)

let test = lotto_select 6 49
let test = lotto_select 0 49 = []
