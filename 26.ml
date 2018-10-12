(* 26. Generate the combinations of K distinct objects
 * chosen from the N elements of a list. (medium)
 *
 * In how many ways can a committee of 3 be chosen from a group of 12 people?
 * We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes
 * the well-known binomial coefficients).
 * For pure mathematicians, this result may be great.
 * But we want to really generate all the possibilities in a list.
 *
 *    C(12, 3) = 12 * 11 * 10 / (3 * 2) = 220
 *)

let rec extract k list =
  if k <= 0 then [[]]
  else match list with
    | [] -> []
    | x :: xs ->
      let with_x = List.map (fun l -> x :: l) (extract (k-1) xs) in
      let without_x = extract k xs in
      with_x @ without_x

let test = extract 2 ["a";"b";"c";"d"]
= [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
