(* 35. Determine the prime factors of a given positive integer. (medium)
 *
 * Construct a flat list containing the prime factors in ascending order. *)

let factors n =
  let rec aux d n =
    if n = 1 then [] else
    if n mod d = 0 then d :: aux d (n / d) else aux (d+1) n
  in
  aux 2 n

let test = factors 315
           = [3; 3; 5; 7]
