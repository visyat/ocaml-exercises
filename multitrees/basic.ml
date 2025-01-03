type 'a mult_tree = T of 'a * 'a mult_tree list;;

(** Count Nodes in a Multiway Tree ... *)
let rec count_nodes tree = 
  match tree with 
  | T (x, children) -> 
    List.fold_left (fun n t -> n+(count_nodes t)) 1 children;;

(** Determine Internal Path Length of Tree ...  *)
(* sum of all path lengths from root to nodes ... *)
let ipl tree = 
  let rec ipl_level tree level = 
    match tree with 
    | T (x, children) -> 
      List.fold_left (fun acc t -> acc+(ipl_level t (level+1))) level children in
  ipl_level tree 0;;

let t = T ('a',[T ('f', [T ('g', [])]); T ('c', []);T ('b', [T ('d', []); T ('e', [])])]);;
let ipl_test0 = (ipl t = 9);;

(** Construct Bottom-Up Order Sequence of Tree Nodes *)
let rec bottom_up tree = 
  match tree with 
  | T (x, children) -> 
    List.fold_right (fun t acc -> (bottom_up t)@acc) children [x];; 

