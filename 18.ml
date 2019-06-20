(* 18. Extract a slice from a list. (medium)
 *
 * Given two indices, i and k, the slice is the list containing the elements
 * between the i'th and k'th element of the original list (both limits included).
 * Start counting the elements with 0
 * (this is the way the List module numbers elements). *)

let rec skip list = function
  | 0 -> list
  | n -> match list with
    | [] -> []
    | x::xs -> skip xs (n-1)

let test01 = skip [1;2;3;4;5] 0
let test02 = skip [1;2;3;4;5] 1
let test03 = skip [1;2;3;4;5] 2
let test04 = skip [1;2;3;4;5] 3
let test05 = skip [1;2;3;4;5] 7

let rskip list number =
  skip (List.rev list) number |> List.rev

let test11 = rskip [1;2;3;4;5] 0
let test12 = rskip [1;2;3;4;5] 1
let test13 = rskip [1;2;3;4;5] 2
let test14 = rskip [1;2;3;4;5] 3
let test15 = rskip [1;2;3;4;5] 7

let slice list i k =
  let temp = skip list i in
  let n = List.length list - (k + 1) in
  if n > 0 then rskip temp n else temp

let test21 = slice [] 2 6 = []
let test22 = slice ["a"] 2 6 = []
let test23 = slice ["a";"b";"c"] 2 6 = ["c"]

                (* 0   1  |2   3   4   5   6|  7   8   9 *)
let test24 = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
             = ["c"; "d"; "e"; "f"; "g"]
