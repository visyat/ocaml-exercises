type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

(** Count Leaves of Binary Tree ... *)
let rec count_leaves tree = 
match tree with 
| Empty -> 0
| Node (_, Empty, Empty) -> 1
| Node (_, left, right) -> (count_leaves left)+(count_leaves right);;

let example_tree =
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
       Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)));;

let count_leaves_test0 = (count_leaves example_tree=3);;

(** Collect Leaves in List ... *)
let rec leaves tree = 
  match tree with 
  | Empty -> []
  | Node (x, Empty, Empty) -> [x]
  | Node (_, left, right) -> (leaves left)@(leaves right);;

(** Collect Internal Nodes of Binary Tree in List ... *)
let rec internals tree = 
  match tree with 
  | Empty | Node (_, Empty, Empty) -> []
  | Node (i, left, right) -> i::((internals left)@(internals right));;

(** Collect Nodes at Given Level ... *)
let at_level tree level = 
  let rec get_nodes tree curr_level = 
    match tree with 
    | Empty -> []
    | Node (i, _, _) when curr_level=level -> [i]
    | Node (_,left,right) -> (get_nodes left (curr_level+1))@(get_nodes right (curr_level+1)) in 
  get_nodes tree 1;;

