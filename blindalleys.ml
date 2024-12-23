(** 9. Filter Blind Alleys *)
(** returns copy of grammar g with all blind-alley rules – grammar rules
for which it is impossible to derive a string of terminal symbols – removed; 
maintain ordering of retained rules *)

(** --- LOGIC --- *)
(** 
* Filter for "good symbols": symbols with rules containing terminal symbols
* Filter for rules containing only the starting nonterminal symbol, "good" symbols, or terminal symbols
*)

type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;
type awksub_nonterminals =
| Expr | Lvalue | Incrop | Binop | Num;;

let is_terminal symbol = 
match symbol with
| T _ -> true
| N _ -> false;;

let rule_contains_terminal rule = 
match rule with 
| lhs, rhs when List.exists is_terminal rhs -> true
| lhs, rhs -> false;; 

let get_lhs rule = 
match rule with 
| lhs, rhs -> lhs;;

let rec find_good rules sym = 
match rules with
| [] -> sym 
| h::t when rule_contains_terminal h && not (List.mem (get_lhs h) sym) -> find_good t (sym@[(get_lhs h)])
| h::t -> find_good t sym;;

let is_good sym set_rules = 
match sym with 
| sym when List.mem sym (find_good set_rules []) -> true 
| sym -> false;;

let rec good_or_terminal rhs set_rules start = 
match rhs with 
| [] -> true 
| h::t when h = start -> good_or_terminal t set_rules start 
| h::t when is_good h set_rules -> good_or_terminal t set_rules start 
| h::t when is_terminal h -> good_or_terminal t set_rules start 
| h::t -> false;;

let rule_valid rule set_rules start = 
match rule with 
| lhs, rhs when is_good lhs set_rules -> true
| lhs, rhs when good_or_terminal rhs set_rules start -> true 
| lhs, rhs -> false;;

(* let filter_blind_alleys g  *)

(** --- TESTING --- *)
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
let test1 = rule_valid (Expr, [N Num]) awksub_rules Expr;;


(* let awksub_test0 = filter_blind_alleys awksub_grammar = awksub_grammar;;  *)
(* let awksub_test1 = filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules);;  *)

(* let awksub_test2 =
  filter_blind_alleys (Expr,
      [Expr, [N Num];
       Expr, [N Lvalue];
       Expr, [N Expr; N Lvalue];
       Expr, [N Lvalue; N Expr];
       Expr, [N Expr; N Binop; N Expr];
       Lvalue, [N Lvalue; N Expr];
       Lvalue, [N Expr; N Lvalue];
       Lvalue, [N Incrop; N Lvalue];
       Lvalue, [N Lvalue; N Incrop];
       Incrop, [T"++"]; Incrop, [T"--"];
       Binop, [T"+"]; Binop, [T"-"];
       Num, [T"0"]; Num, [T"1"]; Num, [T"2"]; Num, [T"3"]; Num, [T"4"];
       Num, [T"5"]; Num, [T"6"]; Num, [T"7"]; Num, [T"8"]; Num, [T"9"]])
  = (Expr,
     [Expr, [N Num];
      Expr, [N Expr; N Binop; N Expr];
      Incrop, [T"++"]; Incrop, [T"--"];
      Binop, [T "+"]; Binop, [T "-"];
      Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"];
      Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]]);; 

let awksub_test3 = filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) =
  filter_blind_alleys (Expr, List.tl (List.tl awksub_rules));; *)