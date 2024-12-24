(** ------ TEST CASES ------ *)
(** 1. Subset *)
let my_subset_test0 = subset [2;4] [1;2;3;4;5] = true;;
let my_subset_test1 = subset [2;4;6;8] [1;2;3;4;5] = false;;
let my_subset_test2 = subset [1;2;3;4] [1;2;3] = false;;
let my_subset_test3 = subset [1;2;3] [1;2;3;4] = true;;

(** 2. Equal Sets *)
let my_equal_sets_test0 = equal_sets [1;2;3] [1;2;3] = true;;
let my_equal_sets_test1 = equal_sets [1;2;3] [1;2;3;4] = false;;
let my_equal_sets_test2 = equal_sets [1;2;3] [2;2;2] = false;;
let my_equal_sets_test3 = equal_sets [5;4;2;1;3] [1;2;3;4;5] = true;;

(** 3. Set Union *)
let my_set_union_test0 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3] = true;;
let my_set_union_test1 = equal_sets (set_union [1;2] [1;2;3]) [1;2;3] = true;;
let my_set_union_test2 = equal_sets (set_union [1;3;5;7] [2;4;6]) [1;2;3;4;5;6;7] = true;;

(** 4. Union of All Sets *)
let my_set_all_union_test0 = equal_sets (set_all_union [[1;2];[1;3];[2;3;4]]) [1;2;3;4] = true;;
let my_set_all_union_test1 = equal_sets (set_all_union [[1;3;5;7];[2;4;6;8];[3;6;9;12]]) [1;2;3;4;5;6;7;8;9;12] = true;;
let my_set_all_union_test2 = equal_sets (set_all_union [[1];[2];[3];[4];[5]]) [1;2;3;4;5] = true;;

(** 6. Computed Fixed Point *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> (x/2)+1) 10 = 2;; (** 10, 6, 4, 3, 2, 2, 2 ... *)
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> (x+4)/2) 10 = 4;; (** 10, 7, 5, 4, 4, 4, ... *)

(** 7. Computed Periodic Point *)
let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> (x/2)+1) 1 10 = 2;;
let my_computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> (x+4)/2) 1 10 = 4;;
let my_computed_periodic_point_test2 = computed_periodic_point (=) (fun x -> (x+3)/2) 2 10 = 3;;
let my_computed_periodic_point_test3 = computed_periodic_point (=) (fun x -> ((2*x)+4)/3) 1 10 = 4;;

(** 8. Longest Sequence *)
let my_whileseq_test0 = whileseq (fun x -> x+1) (fun x -> x < 4) 1 = [1;2;3];;
let my_whileseq_test1 = whileseq (fun x -> 2*x+1) (fun x -> x < 100) 2 = [2;5;11;23;47;95];;

(** 9. Filter Blind Alleys *)
type test_nonterminals = 
| S | A | B | C | D;;

let test0_rules = 
  [S, [N A];
   S, [N B];
   A, [N C; T"1"; N C];
   B, [N C; T"0"; N C];
   C, [T"0"; N C; T"1"];
   C, [T"1"; N C; T"0"];
   C, [T"e"];
  ];;
let test0_grammar = S, test0_rules;;
let my_filter_blind_alleys_test0 = filter_blind_alleys test0_grammar = test0_grammar;;

let test1_rules = 
  [S, [N A];
   S, [N B];
   D, [N D];
   A, [N C; T"1"; N C];
   B, [N C; T"0"; N C];
   D, [N S; N D; N S];
   D, [N D; N D];
   C, [T"0"; N C; T"1"];
   C, [T"1"; N C; T"0"];
   C, [T"e"];
  ];;
let test1_grammar = S, test1_rules;;
let my_filter_blind_alleys_test1 = filter_blind_alleys test1_grammar = test0_grammar;;