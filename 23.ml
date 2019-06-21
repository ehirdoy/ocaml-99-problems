(* 23. Extract a given number of randomly selected elements from a list. (medium)
 *
 * The selected items shall be returned in a list.
 * We use the Random module but do not initialize it with Random.self_init
 * for reproducibility. *)

let rec print_intlist = function
  | [] -> Printf.printf "\n"
  | x::xs -> Printf.printf "%d " x; print_intlist xs

let rand_select list nr =
  if List.length list <= nr then list else
    let rec aux acc =
      if List.length acc >= nr then acc else
        let x = Random.int (List.length list) in
        if List.mem x acc then aux acc else aux (x::acc)
    in
    let index = aux [] |> List.sort (fun a b -> b - a) in
    print_intlist index;
    List.fold_left (fun acc el -> (List.nth list el)::acc) [] index


let test1 = rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3
let test2 = rand_select ["a";"b"] 3
let test3 = rand_select ["a"] 3
let test4 = rand_select [] 3 = []
