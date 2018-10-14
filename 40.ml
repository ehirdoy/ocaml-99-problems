(* 40. Goldbach's conjecture. (medium)
 *
 * Goldbach's conjecture says that every positive even number greater than 2 is
 * the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous
 * facts in number theory that has not been proved to be correct in the general case.
 * It has been numerically confirmed up to very large numbers. Write a function to
 * find the two prime numbers that sum up to a given even integer. *)

let is_prime n =
  let rec is_not_divisor d =
    if d = n then true
    else if n mod d = 0 then false
    else is_not_divisor (d + 1)
  in
  n != 1 && is_not_divisor 2

let rec seq = function | 0 -> [] | x -> x :: seq (x - 1)

let goldbach sum =
  let rec aux = function
    | [] -> (0, 0)
    | x :: xs ->
      let y = sum - x in
      if List.mem y xs then (x, y) else aux xs
  in
  seq sum
  |> List.filter is_prime
  |> List.sort compare
  |> aux

let test = goldbach 28
           = (5, 23)
