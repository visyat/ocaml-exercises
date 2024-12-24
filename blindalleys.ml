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

let is_terminal symbol = 
match symbol with
| T _ -> true
| N _ -> false;;

let rec rhs_all_terminal_good rhs good = 
match rhs with 
| [] -> true
| h::t when is_terminal h -> rhs_all_terminal_good t good
| h::t when List.mem h good -> rhs_all_terminal_good t good
| _ -> false;;

let rule_all_terminal_good rule good = 
match rule with 
| lhs, rhs when rhs_all_terminal_good rhs good -> true 
| _ -> false;;

let get_lhs rule = 
match rule with 
| lhs, rhs -> lhs;;

let rec pass rules good = 
match rules with 
| [] -> good
| h::t when rule_all_terminal_good h good && not (List.mem (N (get_lhs h)) good) -> pass t (good@[N (get_lhs h)])
| h::t -> pass t good;;

let rec pass_loop rules new_good old_good = 
match new_good with 
| new_good when new_good = old_good -> new_good
| new_good -> pass_loop rules (pass rules new_good) new_good;;

let rec rule_filter rule set_rules = 
match rule with 
| lhs, rhs when (List.mem (N lhs) (pass_loop set_rules (pass set_rules []) [])) && (rhs_all_terminal_good rhs (pass_loop set_rules (pass set_rules []) [])) -> true
| _ -> false;;

let rec filter_rules rules1 rules2 filtered = 
match rules1 with
| [] -> filtered
| h::t when (rule_filter h rules2) -> filter_rules t rules2 (filtered@[h])
| h::t -> filter_rules t rules2 filtered;;

let filter_blind_alleys g = 
match g with 
| start, rules -> start, (filter_rules rules rules []);;

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

pass_loop awksub_rules (pass awksub_rules []) [];;

let awksub_grammar = Expr, awksub_rules;;
let awksub_test0 = filter_blind_alleys awksub_grammar = awksub_grammar;; 
let awksub_test1 = filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules);; 

let awksub_test2 =
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

let awksub_test3 = filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) = filter_blind_alleys (Expr, List.tl (List.tl awksub_rules));;
