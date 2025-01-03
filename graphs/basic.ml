(** Conversions Between Graph Formats ...  *)
type 'a graph_term = {nodes : 'a list;  edges : ('a * 'a) list};;

(* graph-term TO edge-clause *)
let conv_gt_ec graph = 
  match graph with 
  | {nodes; edges} -> edges;;

let example_graph = {nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = [('h', 'g'); ('k', 'f'); ('f', 'b'); ('f', 'c'); ('c', 'b')]};;  

(* graph-term TO adjacency-list *)
let conv_gt_al graph = 
  let rec get_edges node edges = 
    match edges with 
    | [] -> []
    | (i,j)::t when i=node -> j::(get_edges node t)
    | _::t -> get_edges node t in
  let rec aggr nodes edges = 
    match nodes with 
    | [] -> []
    | h::t -> (h,(get_edges h edges))::(aggr t edges) in 
  match graph with 
  | {nodes;edges} -> aggr nodes edges;;
let test_gt_al = conv_gt_al example_graph;;

(* adjacency-list TO human-friendly *)
let conv_al_hf graph = 
  let rec iter_over_adj x adj = 
    match adj with 
    | [] -> ""
    | h::t -> (x^"-"^(String.make 1 h))^" "^(iter_over_adj x t) in
  match (List.fold_left (fun acc n -> 
    match n with 
    | x, [] -> acc^(String.make 1 x)^" "
    | x, adj -> acc^(iter_over_adj (String.make 1 x) adj)
    ) "" graph) with 
  | out -> String.trim out;;
let test_al_hf = conv_al_hf test_gt_al;;

(* human-friendly TO graph-term *)
let conf_hf_gt graph = 
  let rec get_nodes graph = 
    let rec unfiltered segments = 
      match segments with 
      | [] -> []
      | h::t -> match (String.split_on_char '-' h) with 
        | s -> (List.fold_left (fun acc s -> acc@[s]) [] s)@(unfiltered t) in
    let rec filter_duplicates nodes seen = 
      match nodes with 
      | [] -> seen 
      | h::t when (List.mem h seen) -> filter_duplicates t seen
      | h::t -> filter_duplicates t (seen@[h]) in 
    filter_duplicates (unfiltered (String.split_on_char ' ' graph)) [] in
  let get_edges graph = 
    let rec extract segments = 
      match segments with 
      | [] -> []
      | h::t -> 
        match String.split_on_char '-' h with 
        | [x;y] -> (x,y)::(extract t)
        | _ -> extract t in
    extract (String.split_on_char ' ' graph) in 
  {nodes = get_nodes graph; edges = get_edges graph};;
