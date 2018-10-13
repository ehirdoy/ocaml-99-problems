(* 28. Sorting a list of lists according to length of sublists. (medium)
 *
 *  We suppose that a list contains elements that are lists themselves.
 *  The objective is to sort the elements of this list according to their length.
 *  E.g. short lists first, longer lists later, or vice versa.
 *
 *  Again, we suppose that a list contains elements that are lists themselves.
 *  But this time the objective is to sort the elements of this list
 *  according to their length frequency;
 *  i.e., in the default, where sorting is done ascendingly,
 *  lists with rare lengths are placed first,
 *  others with a more frequent length come later. *)

let length_sort list =
  let f e1 e2 = List.length e1 - List.length e2 in
  List.sort f list

let test = length_sort [["a";"b";"c"];
                        ["d";"e"];
                        ["f";"g";"h"];
                        ["d";"e"];
                        ["i";"j";"k";"l"];
                        ["m";"n"];
                        ["o"]]
           = [["o"];
              ["d"; "e"];
              ["d"; "e"];
              ["m"; "n"];
              ["a"; "b"; "c"];
              ["f"; "g"; "h"];
              ["i"; "j"; "k"; "l"]]


(* Sorting according to length frequency : prepend frequency, sort,
   remove frequency. Frequencies are extracted by sorting lengths
   and applying RLE to count occurences of each length (see problem
   "Run-length encoding of a list.") *)
let rle list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (count + 1) acc t
      else aux 0 ((a, count + 1) :: acc) t in
  aux 0 [] list


let frequency_sort lists =
  let lengths = List.map List.length lists in
  let freq = rle (List.sort compare lengths) in
  List.map (fun list -> List.assoc (List.length list) freq , list) lists
  |> List.sort (fun a b -> compare (fst a) (fst b))
  |> List.map snd

let test = frequency_sort [["a";"b";"c"];
                           ["d";"e"];
                           ["f";"g";"h"];
                           ["d";"e"];
                           ["i";"j";"k";"l"];
                           ["m";"n"];
                           ["o"]]
           = [["i"; "j"; "k"; "l"]; (* 4:1 *)
              ["o"];                (* 1:1 *)
              ["a"; "b"; "c"];      (* 3:2 *)
              ["f"; "g"; "h"];      (* 3:2 *)
              ["d"; "e"];           (* 2:2 *)
              ["d"; "e"];           (* 2:2 *)
              ["m"; "n"]]           (* 2:2 *)
