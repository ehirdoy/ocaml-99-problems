(* 25. Generate a random permutation of the elements of a list. (easy) *)

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
  aux n [] max

let permutation lst =
  let len = List.length lst in
  let index = random_intlist len len in
  let rec aux index lst = match index with
    | [] -> []
    | x::xs -> (List.nth lst x) :: (aux xs lst) in
  aux index lst

let test = permutation ["a"; "b"; "c"; "d"; "e"; "f"]
                  (* = ["a"; "e"; "f"; "b"; "d"; "c"] *)
