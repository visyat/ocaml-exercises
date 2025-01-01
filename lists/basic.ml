(** Get Tail of a List *)
(* with helper function ... *)
let rec last_help a n = 
match a with 
| [] -> n 
| h::t -> last_help t h;;
let last a = last_help a 0;;

(* without ... *)
let rec last a = 
match a with 
| [] -> None
| [x] -> Some x
| _::t -> last t;;

(** Last 2 elements of list *)
let rec last_two a = 
match a with 
| [] | [_] -> None (* if use [x], variable x has to appear on both sides of the OR ... *)
| [x;y] -> Some (x,y) (* careful to use the semicolon for list members *)
| _::t -> last_two t;;

(** Nth element of list ... *)
let rec at n list = 
match list with 
| [] -> None
| h::t -> 
  match n with 
  | 0 -> Some h 
  | n -> at (n-1) t;;

let at_test0 = at 2 ["a"; "b"; "c"; "d"; "e"];;
let at_test1 = at 2 ["a"];;

(** Length of a list ... *)
(* external helper method ...  *)
let rec length_help list n = 
match list with 
| [] -> n 
| _::t -> length_help t (n+1);;

let rec length list = length_help list 0;;

(* helper method written inside final outer function ... *)
let length list = 
  let length_help list n = 
  match list with 
  | [] -> n 
  | _::t -> length_help t (n+1)
  in length_help list 0;;
let length_test0 = (length ["a"; "b"; "c"] = 3);;
let length_test1 = (length [] = 0);;

(** Reverse a List ... *)
let rev list = 
  let rec rev_store list reversed = 
  match list with
  | [] -> reversed 
  | h::t -> rev_store t (h::reversed)
  in rev_store list [];;
let rev_test0 = (rev [3;2;1] = [1;2;3]);;
let rev_test1 = (rev [5;4;3;2;1] = [1;2;3;4;5]);;

(** Checking Palindrome ... *)
let palindrome list = (list = (rev list));;

let palindrome_test0 = (palindrome [1;2;3;4;5] = false);;
let palindrome_test1 = (palindrome [1;2;3;2;1] = true);;

(** Run-Length Encoding *)
let encode list = 
  let rec encode_help list curr_count curr_val final = 
    match list with 
    | [] -> (final@[(curr_count,curr_val)])
    | h::t when h = curr_val -> encode_help t (curr_count+1) curr_val final
    | h::t -> encode_help t 1 h (final@[(curr_count,curr_val)]) in
  let filter list = 
    match list with 
    | [] -> []
    | h::t -> t 
  in filter (encode_help list 0 "" []);;

let encode_test0 = (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;

(** Modified Run-Length Encoding ... *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;; 

let encode list = 
  let rec encode_help list curr_count curr_val final = 
    match list with 
    | [] -> (final@[(curr_count,curr_val)])
    | h::t when h = curr_val -> encode_help t (curr_count+1) curr_val final
    | h::t -> encode_help t 1 h (final@[(curr_count,curr_val)]) in
  let filter list = 
    match list with 
    | [] -> []
    | h::t -> t in
  let rec modification list fin_list = 
    match list with 
    | [] -> fin_list 
    | h::t -> match h with 
      | (1,v) -> modification t (fin_list@[One v])
      | (n,v) -> modification t (fin_list@[Many (n,v)])
  in modification (filter (encode_help list 0 "" [])) [];;

let mod_encode_test0 = (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
Many (4, "e")]);;

let duplicate list = 
  let rec duplicate_help list fin_list = 
    match list with 
    | [] -> fin_list
    | h::t -> duplicate_help t (fin_list@[h;h]) in
  duplicate_help list [];;
let duplicate_test0 = (duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);;

let split list k = 
  let rec split_help list k first = 
    match list with 
    | [] -> (first, [])
    | h::t when k = 0 -> (first,h::t)
    | h::t -> split_help t (k-1) (first@[h]) in
  split_help list k [];;
let split_test0 = (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));;
let split_test1 = (split ["a"; "b"; "c"; "d"] 5 = (["a"; "b"; "c"; "d"], []));;

let remove_at k list = 
  let rec remove_help k list first = 
    match list with 
    | [] -> first 
    | h::t when k=0 -> first@t 
    | h::t -> remove_help (k-1) t (first@[h]) in
  remove_help k list [];;
let remove_at_test0 = (remove_at 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"]);;
