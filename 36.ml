(* 36. Determine the prime factors of a given positive integer (2). (medium)
 *
 * Construct a list containing the prime factors and their multiplicity.
 * Hint: The problem is similar to problem Run-length encoding of a list
 * (direct solution). *)

let factors n =
  (* prime factors of n, starting d *)
  let rec aux d n =
    if n = 1 then [] else
    if n mod d = 0 then
      match aux d (n / d) with
      | (x, n) :: xs when x = d -> (x, n+1) :: xs
      | xs -> (d, 1) :: xs
    else aux (d+1) n
  in
  aux 2 n

let test = factors 315
(* = [(3, 2); (5, 1); (7, 1)] *)
