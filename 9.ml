(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack lst =
  let rec aux tmp acc src = match src with
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> (x::tmp)::acc
    | x::(x'::_ as t) ->
      if x = x' then aux (x::tmp) acc t
      else aux [] ((x::tmp)::acc) t
  in
  List.rev (aux [] [] lst)

let test =
  pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
  [["a"; "a"; "a"; "a"];
   ["b"];
   ["c"; "c"];
   ["a"; "a"];
   ["d"; "d"];
   ["e"; "e"; "e"; "e"]]
