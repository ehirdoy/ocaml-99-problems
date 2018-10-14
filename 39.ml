(* 39. A list of prime numbers. (easy)
 *
 * Given a range of integers by its lower and upper limit,
 * construct a list of all prime numbers in that range. *)

let seq n0 n1 =
  let rec aux acc x = if x = n1 then acc else aux (x :: acc) (x+1) in
  aux [] n0

let is_prime n =
  let rec is_not_divisor d =
    if d = n then true
    else if n mod d = 0 then false
    else is_not_divisor (d + 1)
  in
  n != 1 && is_not_divisor 2

let all_primes n0 n1 =
  List.fold_left (fun acc el -> if is_prime el then el::acc else acc) [] (seq n0 n1)

let test = List.length (all_primes 2 7920)
           = 1000
