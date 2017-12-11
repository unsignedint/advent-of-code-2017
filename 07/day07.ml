(* day 07 *)

open Printf
open ExtString

let file = "input.txt"


let re_full = Str.regexp "\\([a-z]+\\) (\\([0-9]+\\))\\( -> \\(.*\\)\\)?" ;;
let re_pass_1 = Str.regexp "\\([a-z]+\\) (\\([0-9]+\\))" ;;
let re_pass_2 = Str.regexp ".* -> \\(.*\\)$" ;;


module Nodes = Map.Make(String)
module Parents = Map.Make(String)  (* a node -> parents mapping *)
module Children = Map.Make(String)  (* a node -> children mapping *)

let print_parent k v =
  printf "%s => %s\n" k (String.join "," v)

let part_1_process lines =
  let process_line_a line acc =
    if Str.string_match re_pass_1 line 0 then
      Nodes.add (Str.matched_group 1 line) (int_of_string (Str.matched_group 2 line)) acc
    else
      acc in
  let process_line_b line acc =
    if Str.string_match re_pass_2 line 0 then (
      (* print_endline ("zz=" ^ (Str.matched_group 1 line)); *)
      let _ = Str.string_match re_full line 0 in
      let v = Str.matched_group 1 line in
      (* let parents = List.fold_right (fun x acc -> Parents.add x v acc) (List.map String.strip (Str.split (Str.regexp ", ") (Str.matched_group 4 line))) (fst acc) in *)
      let children = Children.add v (List.map String.strip (Str.split (Str.regexp ", ") (Str.matched_group 4 line))) acc in
      children
      (* List.fold_right (fun x acc -> Children.add x v acc) (List.map String.strip (Str.split (Str.regexp ", ") (Str.matched_group 4 line))) acc *)
    )
    else
      acc in
  let all_nodes = List.fold_right process_line_a lines Nodes.empty in
  let children_nodes = List.fold_right process_line_b lines Children.empty in
  all_nodes, children_nodes

(* find "uneven" level and correct it *)
(* what we need to do is a depth-first search, post-order *)
let dfs root_key nodes children =
  let rec aux level key =
    printf "level=%d key=%s\n" level key ;
    let root_weight = Nodes.find key nodes in
    if Children.mem key children = false then
      (* return leaf weight *)
      root_weight
    else
      (* traverse deeper *)
      let successors = Children.find key children in
      let branch_weights = List.map (aux (level+1)) successors in
      let a, b = List.partition (fun x -> x = List.hd branch_weights) branch_weights in
      let good_weight = (
        printf "a=%s => b=%s\n" (String.join "," (List.map string_of_int a)) (String.join "," (List.map string_of_int b)) ;
        if List.length a = 1 then List.hd b else List.hd a
      ) in
      if List.exists (fun (x, y) -> x <> good_weight) (List.combine branch_weights successors) then (
        let bad_guy = List.find (fun (x, y) -> x <> good_weight) (List.combine branch_weights successors) in
        let weight_difference = good_weight - (fst bad_guy) in
        printf "correction => %s = %d\n" (snd bad_guy) ((Nodes.find (snd bad_guy) nodes) + weight_difference) ;
        failwith "foo" )  (* kill the program when we find the error *)
      else
        root_weight + List.fold_left (fun acc x -> acc + x) 0 branch_weights
  in
  let _ = aux 0 root_key in ()

let () =
  let raw_input = List.map String.strip (Std.input_list (open_in file)) in
  let all_nodes, children_nodes = part_1_process raw_input in
  Children.iter print_parent children_nodes ;
  (* dfs "tknk" all_nodes parent_nodes in *)
  dfs "vgzejbd" all_nodes children_nodes
