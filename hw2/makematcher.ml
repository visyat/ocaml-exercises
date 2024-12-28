(** 3. Produce Matcher for CFG *)
type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;

let is_terminal symbol = 
match symbol with
| T _ -> true
| N _ -> false;;

let extract_nonterminal sym = 
match sym with 
| N x -> x
| T _ -> failwith "Not a nonterminal";;
let extract_terminal sym = 
match sym with 
| T x -> x
| N _ -> failwith "Not a terminal";;

let rec iterate_rules rules trace pred frag = 
match rules with 
| [] -> trace 
| h::t -> iterate_rules t (trace@(iterate_rule_terms h [] pred frag)) pred frag
and iterate_rule_terms rule trace pred frag = 
match rule with 
| [] -> trace
| h::t when is_terminal h && (List.mem (extract_terminal h) frag) -> trace@[extract_terminal h]
| h::t when not (is_terminal h) -> iterate_rule_terms t (trace@(iterate_rules (pred (extract_nonterminal h)) [] pred frag)) pred frag
| h::t -> iterate_rule_terms t trace pred frag;;

(* should output a function of the type fun (accept, frag) -> ... *)

(** --- TESTING --- *)
type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num;;

let accept_all string = Some string;;
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x;;

let awkish_grammar =
(Expr,
  function
    | Expr ->
      [[N Term; N Binop; N Expr];
      [N Term]]
    | Term ->
      [[N Num];
      [N Lvalue];
      [N Incrop; N Lvalue];
      [N Lvalue; N Incrop];
      [T"("; N Expr; T")"]]
    | Lvalue ->
      [[T"$"; N Expr]]
    | Incrop ->
      [[T"++"];
      [T"--"]]
    | Binop ->
      [[T"+"];
      [T"-"]]
    | Num ->
      [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
      [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]);;
(* testing helper functions ... *)

let test_tl gram frag = 
  match gram with 
  | start, pred -> iterate_rules (pred start) [] pred frag;;

let test0 = test_tl awkish_grammar ["9"];;

(* let test0 = ((make_matcher awkish_grammar accept_all ["ouch"]) = None);; *)
(* let test1 = ((make_matcher awkish_grammar accept_all ["9"]) = Some []);;
let test2 = ((make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"]) = Some ["+"]);;
let test3 = ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"]) = None);; *)