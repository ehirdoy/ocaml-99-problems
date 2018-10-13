(* 33. Determine whether two positive integer numbers are coprime. (easy)
 *
 * Two numbers are coprime if their greatest common divisor equals 1. *)

let coprime m n =
  let rec gcd m n =
    let res = m mod n in
    if res = 0 then n else gcd n res in
  gcd m n = 1

let test = coprime 13 27
           = true
let test = not (coprime 20536 7826)
           = true
