(* Binary Trees
 *
 * A binary tree is either empty or it is composed of a root element and
 *  two successors, which are binary trees themselves.
 *
 * In OCaml, one can define a new
 * type binary_tree that carries an arbitrary value of
 * type 'a (thus is polymorphic) at each node.
 *
 * # type 'a binary_tree =
 *     | Empty
 *     | Node of 'a * 'a binary_tree * 'a binary_tree
 * type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree
 *
 * An example of tree carrying char data is:
 *
 * # let example_tree =
 *     Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
 *          Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;
 * val example_tree : char binary_tree =
 *   Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
 *    Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
 * # let example_int_tree =
 *     Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)),
 *          Node(3, Empty, Node(6, Node(7, Empty, Empty), Empty)));;
 * val example_int_tree : int binary_tree =
 *   Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 *    Node (3, Empty, Node (6, Node (7, Empty, Empty), Empty)))
 *
 * In OCaml, the strict type discipline guarantees that, if you get a valueof
 * type binary_tree, then it must have been created with the two constructors
 * Empty and Node. *)

(* 55. Construct completely balanced binary trees. (medium)
 *
 * In a completely balanced binary tree, the following property holds
 * for every node: The number of nodes in its left subtree and the number
 * of nodes in its right subtree are almost equal, which means their difference
 * is not greater than one.
 *
 * Write a function cbal_tree to construct completely balanced binary trees for
 * a given number of nodes. The function should generate all solutions via
 * backtracking. Put the letter 'x' as information into all nodes of the tree. *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let cbal_tree number =
  []

let test = cbal_tree 4
(* - : char binary_tree list *)=
[Node ('x',Node('x',Empty,Empty),Node('x',Node('x',Empty,Empty),Empty));
 Node ('x',Node('x',Empty,Empty),Node('x',Empty,Node('x',Empty,Empty)));
 Node ('x',Node('x',Node('x',Empty,Empty),Empty),Node('x',Empty, Empty));
 Node ('x',Node('x',Empty,Node('x',Empty,Empty)),Node('x',Empty,Empty))]

let test = List.length(cbal_tree 40)
(*- : int *)= 524288
