(* 27. Group the elements of a set into disjoint subsets. (medium) *)

let rec dump_list = function
  | [] -> Printf.printf "| "
  | x::xs -> Printf.printf "%d " x; dump_list xs
(* let test001 = dump_list [0;1;2;3;4;5]
 * let test002 = dump_list [0]
 * let test002 = dump_list [] *)

let rec dump_llist = function
  | [] -> Printf.printf "\n"
  | x::xs -> dump_list x; dump_llist xs
(* let test011 = dump_llist [[0;1];[2]] *)

let rec dump_lllist = function
  | [] -> Printf.printf "--------------\n"
  | x::xs -> dump_llist x; dump_lllist xs
(* let test012 = dump_lllist [[[0;1];[2]];
 *                            [[0;2];[1]];
 *                            [[1;2];[0]]] *)

(* combo `m` `n` : list 'n' combination from `m` *)
let rec combo list nr = match list, nr with
  | _, 0 -> [[]]
  | [], n -> [[]]
  | l, n when n = List.length l -> [l]
  | x::xs, n ->
    let wtx = List.map (fun el -> x::el) (combo xs (n-1)) in
    let wox = combo xs n in
    wtx @ wox

let test04 = combo [0;1;2] 0 = [[]]
let test05 = combo [0;1;2] 1 = [[0];[1];[2]]
let test06 = combo [0;1;2] 2 = [[0;1];[0;2];[1;2]]
let test07 = combo [0;1;2] 3 = [[0;1;2]]
let test08 = combo (List.init 5 (fun x -> x)) 3
             = [[0; 1; 2]; [0; 1; 3]; [0; 1; 4]; [0; 2; 3]; [0; 2; 4];
                [0; 3; 4]; [1; 2; 3]; [1; 2; 4]; [1; 3; 4]; [2; 3; 4]]

(* Returns a combo followed by rest of elements in List *)
let rec combo2 list nr = match list, nr with
  | l, 0 -> [([], l)]
  | [], n -> [([], [])]
  | l, n when n = List.length l -> [(l, [])]
  | x::xs, n ->
    let wtx = List.map (fun (cl,res) -> x::cl, res) (combo2 xs (n-1)) in
    let wox = List.map (fun (cl,res) -> cl, x::res) (combo2 xs n) in
    wtx @ wox

let test14 = combo2 [0;1;2] 0 = [([],[0;1;2])]
let test15 = combo2 [0;1;2] 1 = [([0], [1;2]);
                                 ([1], [0;2]);
                                 ([2], [0;1])]
let test16 = combo2 [0;1;2] 2 = [([0;1], [2]);
                                 ([0;2], [1]);
                                 ([1;2], [0])]
let test17 = combo2 [0;1;2] 3 = [([0;1;2],[])]
let test18 = combo2 (List.init 5 (fun x -> x)) 3
             = [([0;1;2], [3;4]);
                ([0;1;3], [2;4]);
                ([0;1;4], [2;3]);
                ([0;2;3], [1;4]);
                ([0;2;4], [1;3]);
                ([0;3;4], [1;2]);
                ([1;2;3], [0;4]);
                ([1;2;4], [0;3]);
                ([1;3;4], [0;2]);
                ([2;3;4], [0;1])]


(* 1. In how many ways can a group of 9 people work in 3 disjoint
 *   subgroups of 2, 3 and 4 persons? Write a function that generates
 *   all the possibilities and returns them in a list.*)

(* let rec prepend l ll =
 *   List.map (fun el -> l::el) ll
 *
 * let testaaa = prepend [5;6] [[[0;1];[2]];
 *                              [[0;2];[1]];
 *                              [[1;2];[0]];]
 *               = [[[5;6];[0;1];[2]];
 *                  [[5;6];[0;2];[1]];
 *                  [[5;6];[1;2];[0]];]
 *
 * let rec group list = function
 *   | [] -> []
 *   | x::xs ->
 *     List.map (fun (l, rem) ->
 *         prepend l (group rem xs))
 *       (combo2 list x)
 *
 * let test21 = group [0;1;2;3;4] [] = []
 * let test22 = group [0;1;2;3;4] [0]
 * let test23 = group [0;1;2;3;4] [1] = [[[0]];[[1]];[[2]];[[3]];[[4]]]
 * let test24 = group [0;1;2] [2;1]
 * let test24 = group [0;1;2;3;4] [2;2;1]
 *
 * let test25 = group ["a";"b";"c";"d"] [2;1]
 * = [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 *    [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 *    [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 *    [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]] *)

(* 2. Generalize the above function in a way that we can specify a list
 *    of group sizes and the function will return a list of groups. *)

(* This implementation is less streamlined than the one-extraction
    version, because more work is done on the lists after each
    transform to prepend the actual items. The end result is cleaner
    in terms of code, though. *)

let group list sizes =
  let initial = List.map (fun size -> size, []) sizes in

  (* The core of the function. Prepend accepts a list of groups,
       each with the number of items that should be added, and
       prepends the item to every group that can support it, thus
       turning [1,a ; 2,b ; 0,c] into [ [0,x::a ; 2,b ; 0,c ];
       [1,a ; 1,x::b ; 0,c]; [ 1,a ; 2,b ; 0,c ]]

       Again, in the prolog language (for which these questions are
       originally intended), this function is a whole lot simpler.  *)
  let prepend p list =
    let emit l acc = l :: acc in
    let rec aux emit acc = function
      | [] -> emit [] acc
      | (n,l) as h :: t ->
        let acc =
          if n > 0 then emit ((n-1, p::l) :: t) acc else acc in
        aux (fun l acc -> emit (h :: l) acc) acc t
    in
    aux emit [] list
  in
  let rec aux = function
    | [] -> [ initial ]
    | h :: t -> List.concat (List.map (prepend h) (aux t))
  in
  let all = aux list in
  (* Don't forget to eliminate all group sets that have non-full
       groups *)
  let complete = List.filter (List.for_all (fun (x,_) -> x = 0)) all in
  List.map (List.map snd) complete

let test = group ["a";"b";"c";"d"] [2;1]
(* = [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 *    [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 *    [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 *    [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]] *)
