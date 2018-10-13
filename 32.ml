(* 32. Determine the greatest common divisor of two positive integer numbers. (medium)
 *
 * Use Euclid's algorithm. *)

let rec gcd m n =
  let res = m mod n in
  if res = 0 then n else gcd n res

let test = gcd 13 27 = 1
let test = gcd 20536 7826 = 2
