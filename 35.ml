(* 35. Determine the prime factors of a given positive integer. (medium)
 *
 * Construct a flat list containing the prime factors in ascending order. *)

let rec seq = function
  | 0 -> []
  | x -> x :: seq (x - 1)

let is_prime nr =
  if nr = 1 then false else
    let rec aux x =
      if x = nr then true
      else if (nr mod x = 0) then false
      else aux (x+1) in
    aux 2

let n1 = is_prime 1
let n2 = is_prime 2
let n3 = is_prime 3
let n4 = is_prime 4
let n5 = is_prime 5
let n6 = is_prime 6
let n7 = is_prime 7

let prime_numbers nr =
  seq nr
  |> List.fold_left
    (fun acc el -> if is_prime el then el :: acc else acc) []

let _ = prime_numbers 18

let factors nr =
  let rec f acc el =
    if nr mod el = 0 then f (el :: acc) (el * el) else acc in
  List.fold_left f [] (prime_numbers nr)

let test = factors 315
           (* = [3; 3; 5; 7] *)
