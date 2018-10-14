(* 39. A list of prime numbers. (easy)
 *
 * Given a range of integers by its lower and upper limit,
 * construct a list of all prime numbers in that range. *)

let is_prime n =
  let rec is_not_divisor d =
    if d = n then true
    else if n mod d = 0 then false
    else is_not_divisor (d + 1)
  in
  n != 1 && is_not_divisor 2

let rec all_primes n0 n1 =
  if n0 > n1 then [] else
  if is_prime n0 then n0 :: (all_primes (n0+1) n1) else (all_primes (n0+1) n1)

let test = List.length (all_primes 2 7920)
           = 1000
