(* 38. Compare the two methods of calculating Euler's totient function. (easy)
 *
 * Use the solutions of problems "Calculate Euler's totient function φ(m)" and
 * "Calculate Euler's totient function φ(m) (improved)" to compare the algorithms.
 * Take the number of logical inferences as a measure for efficiency.
 * Try to calculate φ(10090) as an example. *)

let phi nr =
  let rec seq = function | 0 -> [] | x -> x :: seq (x - 1) in
  let coprime m n =
    let rec gcd m n =
      let res = m mod n in
      if res = 0 then n else gcd n res
    in gcd m n = 1 in
  seq nr
  |> List.fold_left
    (fun acc el -> if coprime el nr then acc + 1 else acc) 0

let phi_improved nr =
  let factors n =
    let rec aux d n = (* prime factors of n, starting d *)
      if n = 1 then [] else
      if n mod d = 0 then
        match aux d (n / d) with
        | (x, n) :: xs when x = d -> (x, n+1) :: xs
        | xs -> (d, 1) :: xs
      else aux (d+1) n
    in
    aux 2 n in
  let rec pow n p = if p < 1 then 1 else n * pow n (p-1) in
  factors nr
  |> List.fold_left (fun acc (p, m) -> ((p-1) * pow p (m - 1)) * acc) 1

let timeit f a =
  let t0 = Unix.gettimeofday () in
  ignore (f a);
  let t1 = Unix.gettimeofday () in
  t1 -. t0

let test = timeit phi 10090
(* = 0.00266885757446289062 *)
let test = timeit phi_improved 10090
(* = 3.2901763916015625e-05 *)
