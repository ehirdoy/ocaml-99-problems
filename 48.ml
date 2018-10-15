(* 48. Truth tables for logical expressions. (medium)
 *
 * Generalize the previous problem in such a way that the logical expression
 * may contain any number of logical variables. Define table in a way that
 * table variables expr returns the truth table for the expression expr,
 * which contains the logical variables enumerated in variables. *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval conf = function
  | Var x -> List.assoc x conf
  | Not e -> not (eval conf e)
  | And (e1, e2) -> eval conf e1 && eval conf e2
  | Or (e1, e2) -> eval conf e1 || eval conf e2

let rec create_table = function
  | [] -> [[]]
  | x :: xs ->
    let prepend x l = List.map (fun el -> x :: el) l in
    let rest = create_table xs in
    (prepend (x, true) rest) @ (prepend (x, false) rest)

let table vars expr =
  create_table vars |> List.map (fun el -> (el, (eval el expr)))

let test = table ["a"] (Var "a")
let test = table ["a";"b"] (Var "a")
let test = table ["a";"b"] (Var "b")

(* ((string * bool) list * bool) list *)
let test =
  table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")))
  =
  [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
   ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]

(* ((string * bool) list * bool) list *)
let test =
  let (a, b, c) = (Var "a", Var "b", Var "c") in
  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))))
  =
  [([("a", true); ("b", true); ("c", true)], true);
   ([("a", true); ("b", true); ("c", false)], true);
   ([("a", true); ("b", false); ("c", true)], true);
   ([("a", true); ("b", false); ("c", false)], false);
   ([("a", false); ("b", true); ("c", true)], false);
   ([("a", false); ("b", true); ("c", false)], false);
   ([("a", false); ("b", false); ("c", true)], false);
   ([("a", false); ("b", false); ("c", false)], false)]
