(** 1. Subset *)
let subset a b = List.for_all (fun x -> List.mem x b) a;;

(** 2. Equal Sets *)
let equal_sets a b = subset a b && subset b a;;

(** 3. Set Union *)
let rec set_union a b = 
match a with 
| [] -> b
| h::t -> h::set_union t b;;

(** 4. Union of All Sets *)
let rec set_all_union a = 
match a with 
| [] -> []
| h::t -> set_union h (set_all_union t);;

(** 5. Russell's Paradox – this is impossible in OCaml. 
Expressions of type 'a list cannot in turn be composed of their own type 
(recursively, with no base type 'a). The resulting error will be produced: 
"This expression has type 'a but an expression was expected of type 'a list. 
The type variable 'a occurs inside 'a list"
*)

(** 6. Computed Fixed Point)
let rec computed_fixed_point eq f x = 
match x with 
| x when eq x (f x) -> x
| x -> computed_fixed_point eq f (f x);;

(** 7. Computed Periodic Point *)
let rec computed_periodic_point eq f p x = 
match x with 
| x when eq x (p_periodic f p x) -> x 
| x -> computed_periodic_point eq f p (f x);;

let rec p_periodic f p x = 
match p with 
| 0 -> x 
| p -> p_periodic f (p-1) (f x);;

(** 8. Longest Sequence *)
(** longest sequence [x, s x, s (s x), s (s (s x)), ...] 
such that p e is true for every element *)

let whileseq s p x = helper s p x [];; 

let rec helper s p x seq = 
match x with 
| x when p x -> helper s p (s x) seq@x 
| x -> seq;;

(** 9. Filter Blind Alleys *)
let filter_blind_alleys g = 