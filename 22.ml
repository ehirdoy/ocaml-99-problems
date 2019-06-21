(* 22. Create a list containing all integers within a given range. (easy)
 *
 * If first argument is greater than second, produce a list in decreasing order. *)

let range start _end =
  let len = abs (_end - start) + 1 in
  let dir =
    if start >= _end then -1 else 1 in
  List.init len (fun index -> start + index * dir)

let test01 = range 1 1 = [1]
let test02 = range 4 9
             = [4; 5; 6; 7; 8; 9]
let test03 = range 9 4
             = [9; 8; 7; 6; 5; 4]
