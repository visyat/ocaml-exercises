(** 3. Produce Matcher for CFG *)
type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;

let matcher start pred frag = 
  let rec get_prefix sym = 
    List.concat (
      List.fold_left
        (fun acc rule -> acc @ [
          List.fold_left
          (fun rule_acc term ->
            match term with
            | T s when List.mem s frag -> rule_acc@[s]
            | T s -> rule_acc
            | N s -> rule_acc@(get_prefix s)
          )
          [] rule
        ])
      [] (pred sym)
    ) in 
  get_prefix start;;
(* 
get_prefix sym -> : outputs a list of rules, each with a list of symbols ...
    fetch pred sym: list of rules with sym as LHS 
    List.fold_left 
      (fun acc rule ->
        acc @ 
        (List.fold_left 
        (fun rule_acc term -> 
          match term with 
          | T sym when List.mem sym frag -> acc@[sym]
          | T sym -> acc
          | N sym -> acc@[get_prefix sym]
        [] rule) : rule is list of symbols
      ) 
    [] (pred sym)
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

let test_tl gram frag = 
  match gram with 
  | start, pred -> matcher start pred frag;;
let test0 = test_tl awkish_grammar ["9"];;