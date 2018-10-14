(* 41. A list of Goldbach compositions. (medium)
 *
 * Given a range of integers by its lower and upper limit,
 * print a list of all even numbers and their Goldbach composition.
 *
 * In most cases, if an even number is written as the sum of two prime numbers,
 * one of them is very small. Very rarely, the primes are both bigger than say 50.
 * Try to find out how many such cases there are in the range 2..3000. *)

#use "40.ml"

let rec seq a b =
  let a = (if a mod 2 = 0 then a else a+1) in
  if a > b then [] else (a :: (seq (a+2) b))

let rec goldbach_list a b =
  if a mod 2 = 1 then goldbach_list (a+1) b
  else (a, (goldbach a)) :: (goldbach_list (a+2) b)

let test = goldbach_list 9 20
= [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
   (20, (3, 17))]

let goldbach_limit a b c =
  goldbach_list a b |> List.filter (fun (_, (x, y)) -> x > c && y > c)

let test = goldbach_limit 1 2000 50
= [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
   (1928, (61, 1867))]
