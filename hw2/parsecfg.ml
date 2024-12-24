(** 1. Convert HW1-Style Grammar to HW2-Style Grammar *)
type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;

let get_lhs rule = 
match rule with 
| lhs, rhs -> lhs;;

let get_rhs rule = 
match rule with 
| lhs, rhs -> rhs;;

(* let rec get_all_lhs rules list_lhs = 
match rules with 
| [] -> list_lhs
| h::t when not(List.mem (get_lhs h) list_lhs) -> get_all_lhs t (list_lhs@[get_lhs h])
| h::t -> get_all_lhs t list_lhs;; *)

let match_rule_group rule sym = 
match rule with 
| lhs, rhs when lhs = sym -> true
| _ -> false;;

let rec group_rhs rules lhs group = 
match rules with 
| [] -> group
| h::t when match_rule_group h lhs -> group_rhs t lhs (group@[get_rhs h])
| h::t -> group_rhs t lhs group;;

let convert_grammar gram1 = 
match gram1 with 
| start, rules -> start, (fun sym -> (group_rhs rules sym []));;

(** 2. Traverse Parse Tree *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal;;

(* handles parent nodes ... 
   if encounters leaf, appends to the leaves list, 
    if encounters tree node, passes children to child handler ... *)
let rec traverse_parent tree leaves = 
match tree with 
| Node (node, children) -> traverse_children children leaves
| Leaf leaf -> leaves@[leaf]
(* handles child nodes (stored in a list) ...
   have to recursively traverse list, passing each node/leaf 
   to the parent handler as it is reached, before continuing to 
   next child in list ...*)
and traverse_children children leaves = 
match children with 
| [] -> leaves 
(* iteration through child nodes ... 
   on each child, pass to parent handler, which appends any leaves 
   found to the list ...*)
| h::t -> traverse_children t (leaves@(traverse_parent h []));;

let parse_tree_leaves tree = traverse_parent tree [];;

(** --- TESTING --- *)
type awksub_nonterminals =
| Expr | Lvalue | Incrop | Binop | Num;;

let awksub_rules =
  [Expr, [T"("; N Expr; T")"];
   Expr, [N Num];
   Expr, [N Expr; N Binop; N Expr];
   Expr, [N Lvalue];
   Expr, [N Incrop; N Lvalue];
   Expr, [N Lvalue; N Incrop];
   Lvalue, [T"$"; N Expr];
   Incrop, [T"++"];
   Incrop, [T"--"];
   Binop, [T"+"];
   Binop, [T"-"];
   Num, [T"0"];
   Num, [T"1"];
   Num, [T"2"];
   Num, [T"3"];
   Num, [T"4"];
   Num, [T"5"];
   Num, [T"6"];
   Num, [T"7"];
   Num, [T"8"];
   Num, [T"9"]];;  
let awksub_grammar = Expr, awksub_rules;;
let new_grammar = convert_grammar awksub_grammar;;

let testTree = Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]);; (*(string, int) parse_tree*) (*= [3; 4; 5];;*) 
let leaves = parse_tree_leaves testTree;;