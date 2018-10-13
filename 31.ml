(* 31. Determine whether a given integer number is prime. (medium) *)

let is_prime n =
  let rec is_not_divisor d =
    if d = n then true
    else if n mod d = 0 then false
    else is_not_divisor (d + 1)
  in
  n != 1 && is_not_divisor 2

let test = not(is_prime 1) = true
let test = is_prime 7 = true
let test = not (is_prime 12) = true
