(* 34. Calculate Euler's totient function φ(m). (medium)
 *
 * Euler's so-called totient function φ(m) is defined as the number of
 * positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.
 *
 * Find out what the value of φ(m) is if m is a prime number.
 * Euler's totient function plays an important role in one of the most widely
 * used public key cryptography methods (RSA). In this exercise you should use
 * the most primitive method to calculate this function (there are smarter ways
 * that we shall discuss later). *)

let coprime m n =
  let rec gcd m n =
    let res = m mod n in
    if res = 0 then n else gcd n res in
  gcd m n = 1

let c1 = coprime 1 10
let c2 = coprime 2 10
let c3 = coprime 3 10
let c4 = coprime 4 10
let c5 = coprime 5 10
let c6 = coprime 6 10
let c7 = coprime 7 10
let c8 = coprime 8 10
let c9 = coprime 9 10

let rec seq = function
  | 0 -> []
  | x -> x :: seq (x - 1)

let phi nr =
  seq nr
  |> List.fold_left
    (fun acc el -> if coprime el nr then acc + 1 else acc) 0

let test =  phi 10 = 4
let test =  phi 13 = 12
