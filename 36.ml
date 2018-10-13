(* 36. Determine the prime factors of a given positive integer (2). (medium)
 *
 * Construct a list containing the prime factors and their multiplicity.
 * Hint: The problem is similar to problem Run-length encoding of a list
 * (direct solution). *)

let rle list =
  List.fold_left
    (fun acc el ->
       if List.mem_assoc el acc
       then (el, (List.assoc el acc) + 1) :: (List.remove_assoc el acc)
       else  (el, 1) :: acc)
    [] list

let factors n =
  let rec aux d n =
    if n = 1 then [] else
    if n mod d = 0 then d :: aux d (n / d) else aux (d+1) n
  in
  aux 2 n
  |> List.sort (fun e1 e2 -> e2 - e1)
  |> rle

let test = factors 315
(* = [(3, 2); (5, 1); (7, 1)] *)
