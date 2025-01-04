(** Flatten Nested List Structure ... *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;
let rec flatten list = 
  match list with 
  | [] -> []
  | h::t -> 
    match h with 
    | One x -> x::(flatten t)
    | Many sub -> (flatten sub)@(flatten t);;

(** Eliminating Consecutive Duplicates in List ...  *)
let rec compress list = 
  match list with 
  | [] -> []
  | h::(n::_ as t) when h=n -> (compress t)
  | h::t -> h::(compress t);;