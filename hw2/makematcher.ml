(** 3. Produce Matcher for CFG *)
type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;

(* iterate over the rules of the starting symbol ... 
   first one that produces valid prefix-suffix such that accept suffix = true returns *)
(* for each rule implement recursive checking below ... *)
(* rule -> list of terms 
   - iterate over terms ...
    - if term: T -> 
      - if term is next in frag: append to prefix 
      - else: do not append 
    - if term: N -> 
      - get (pred term): list of rules 
      - iterate over (pred terms) -> 
         pass each rule into top-level recursion 
 *)
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

(* let test_tl gram frag = 
  match gram with 
  | start, pred -> iterate_rules (pred start) [] pred frag;; *)

(* let test0 = test_tl awkish_grammar ["9"];; *)

(* let test0 = ((make_matcher awkish_grammar accept_all ["ouch"]) = None);;
let test1 = ((make_matcher awkish_grammar accept_all ["9"]) = Some []);;
let test2 = ((make_matcher awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"]) = Some ["+"]);;
let test3 = ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"]) = None);; *)