(* day 13 *)

open Printf
open ExtString

let file = "input.txt"

type direction = Down | Up

module Graph = Map.Make(struct
  type t = int
  let compare = compare
end)

let process_line l =
  let a = List.map int_of_string (Str.split_delim (Str.regexp ": ") l) in
  List.hd a, List.hd (List.tl a)

let build_network lines =
  let add_to_graph l g =
    let depth, range = process_line l in
    Graph.add depth ((range - 1) * 2) g in
  List.fold_right add_to_graph lines Graph.empty

let new_find_solution ?delay:(delay=0) g =
  Graph.fold (fun k v acc -> if (delay + k) mod v = 0 then k :: acc else acc) g []

let rec solve g x =
  let sol = new_find_solution g ~delay:x in
  if List.length sol = 0 then x, sol
  else solve g (x + 1)

let () =
  let raw_lines = List.map String.strip (Std.input_list (open_in file)) in
  let g = build_network raw_lines in
  (* Graph.iter (fun k v -> printf "%d => %d\n" k v) g ; *)
  let convert_to_range x = x / 2 + 1 in
  let part_1 = new_find_solution g in
  printf "part_1 = %s\n" (String.join "," (List.map string_of_int part_1)) ;
  printf "part_1 answer = %d\n" (List.fold_left (fun acc x -> acc + x * (convert_to_range (Graph.find x g))) 0 part_1) ;
  let part_2_delay, part_2 = solve g 1 in
  printf "part_2 answer = %d\n" part_2_delay  ;
