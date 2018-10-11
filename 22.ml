(* 22. Create a list containing all integers within a given range. (easy)
 *
 * If first argument is greater than second, produce a list in decreasing order. *)

let rec range s e =
  if s < e then s::range (s+1) e
  else if s = e then [s]
  else s::range (s-1) e

let test = range 1 1 = [1]
let test = range 4 9
= [4; 5; 6; 7; 8; 9]
let test = range 9 4
= [9; 8; 7; 6; 5; 4]
