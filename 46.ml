(* Logic and Codes
 *
 * Let us define a small "language" for boolean expressions
 * containing variables:
 *
 *   type bool_expr =
 *     | Var of string
 *     | Not of bool_expr
 *     | And of bool_expr * bool_expr
 *     | Or of bool_expr * bool_expr
 *
 * A logical expression in two variables can then be written
 * in prefix notation.
 *
 * For example, (a ∨ b) ∧ (a ∧ b) is written:
 *
 *   # And(Or(Var "a", Var "b"), And(Var "a", Var "b"));;
 *   - : bool_expr = And (Or (Var "a", Var "b"), And (Var "a", Var "b"))
 *
 * 46 & 47. Truth tables for logical expressions (2 variables). (medium)
 *
 * Define a function, table2 which returns the truth table of a given logical
 * expression in two variables (specified as arguments). The return value
 * must be a list of triples containing (value_of_a, balue_of_b, value_of_expr). *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval2 conf = function
  | Var x -> List.assoc x conf
  | Not e -> not (eval2 conf e)
  | And (e1, e2) -> eval2 conf e1 && eval2 conf e2
  | Or (e1, e2) -> eval2 conf e1 || eval2 conf e2

let rec create_table = function
  | [] -> [[]]
  | x :: xs ->
    let prepend x l = List.map (fun el -> x :: el) l in
    let rest = create_table xs in
    (prepend (x, true) rest) @ (prepend (x, false) rest)

let table2 vars expr =
  create_table vars |> List.map (fun el -> (el, (eval2 el expr)))

let test1 = table2 ["x";"y"] (Var "x")
let test2 = table2 ["a";"b"] (And (Var "a", Or (Var "a", Var "b")))
let test3 = table2 ["a";"b"] (Or (Var "a", Or (Var "a", Var "b")))
let test4 = table2 ["a";"b"] (And (Var "a", And (Var "a", Var "b")))
