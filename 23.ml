(* 23. Extract a given number of randomly selected elements from a list. (medium)
 *
 * The selected items shall be returned in a list.
 * We use the Random module but do not initialize it with Random.self_init
 * for reproducibility. *)

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

(* let test = random_intlist 3 8
 * let test = random_intlist 2 2
 * let test = random_intlist 2 9
 * let test = random_intlist 3 5 *)

let rand_select lst nr =
  let rec aux l = function
    | [] -> []
    | x::xs -> (List.nth l x) :: (aux l xs)
  in
  aux lst (random_intlist nr (List.length lst))

let test1 = rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3
let test2 = rand_select ["a";"b"] 3 = ["a";"b"]
let test3 = rand_select ["a"] 3 = ["a"]
let test4 = rand_select [] 3 = []
