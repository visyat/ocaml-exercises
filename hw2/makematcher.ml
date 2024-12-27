(** 3. Produce Matcher for CFG *)
type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;

let is_terminal symbol = 
match symbol with
| T _ -> true
| N _ -> false;;

let rec iterate_rule_terms rule frag prefix pred = 
match rule with 
| [] -> prefix
| h::t when is_terminal h && List.mem h frag -> prefix@[h]
| h::t when is_terminal h -> prefix 
| h::t -> iterate_rule_terms t frag (prefix@[iterate_rules (pred h) frag [] pred]) pred
and iterate_rules rules frag prefix pred = 
match rules with 
| [] -> prefix 
| h::t -> prefix@[iterate_rule_terms h frag [] pred];;

(* start traversing first rule of (prod start) ...
  once get to end, stop and check with accept function *)
let rec top_level start_rules frag pred = 
match start_rules with 
| [] -> None (* if there are no valid prefixes ... *)
| h::t -> iterate_rule_terms h frag [] pred;;
(* once compose a valid prefix, check if suffix can be accepted ...
if yes, return accept suffix; if not, continue to next start_rule *)

let make_matcher gram = 
match gram with
| start, pred -> (fun frag accept -> top_level (pred start) frag pred);;

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

let test0 = ((make_matcher awkish_grammar accept_all ["ouch"]) = None);;
(* let test1 = ((make_matcher awkish_grammar accept_all ["9"]) = Some []);;
let test2 = ((make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"]) = Some ["+"]);;
let test3 = ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"]) = None);; *)