(* 22. Create a list containing all integers within a given range. (easy)
 *
 * If first argument is greater than second, produce a list in decreasing order. *)

let range s e =
  let rec aux high low =
    if high > low then high :: aux (high - 1) low else [low]
  in
  if s < e then List.rev (aux e s) else aux s e

let test = range 1 1 = [1]
let test = range 4 9
= [4; 5; 6; 7; 8; 9]
let test = range 9 4
= [9; 8; 7; 6; 5; 4]
