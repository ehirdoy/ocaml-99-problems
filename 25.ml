(* 25. Generate a random permutation of the elements of a list. (easy) *)

#use "24.ml"

let permutation list =
  let rec aux acc m =
    if m = 0 then acc else
      let swap l n1 n2 =
        let ar = Array.of_list l in
        let temp = ar.(n1) in
        ar.(n1) <- ar.(n2);
        ar.(n2) <- temp;
        Array.to_list ar
      in
      let len = List.length list in
      let n1, n2 = Random.(int len, int len) in
      aux (swap acc n1 n2) (m-1)
  in
  aux list (Random.int 9)

let test = permutation ["a"; "b"; "c"; "d"; "e"; "f"]
                  (* = ["a"; "e"; "f"; "b"; "d"; "c"] *)
