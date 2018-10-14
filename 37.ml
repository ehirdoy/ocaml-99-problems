(* 37. Calculate Euler's totient function φ(m) (improved). (medium)
 *
 * See problem "Calculate Euler's totient function φ(m)" for the definition of
 * Euler's totient function. If the list of the prime factors of a number m is
 * known in the form of the previous problem then the function phi(m) can be
 * efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be
 * the list of prime factors (and their multiplicities) of a given number m.
 * Then φ(m) can be calculated with the following formula:
 *
 * φ(m) = ((p1 - 1) * p1 ** (m1 - 1))
 *      × ((p2 - 1) × p2 ** (m2 - 1))
 *      × ((p3 - 1) × p3 ** (m3 - 1)) × ⋯
 *)

let factors n = (* copied *)
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

let _ = factors 10 (* [(2, 1); (5, 1)] = ((2-1)*2**0) * ((5-1)*5**0) = 4 *)
let _ = factors 13 (* [(13, 1)] = (13-1)*13**0 = 12 *)

let rec pow n p = if p < 1 then 1 else n * pow n (p-1) (* copied *)

let phi_improved nr =
  factors nr
  |> List.fold_left (fun acc (p, m) -> ((p-1) * pow p (m - 1)) * acc) 1

let test = phi_improved 10
= 4
let test = phi_improved 13
= 12
