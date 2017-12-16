(* day 12 *)

open Printf
open ExtString

let file = "input2.txt"

let re_full = Str.regexp "\\([0-9]+\\) <-> \\(.*\\)" ;;

module Graph = Map.Make(struct
  type t = int
  let compare = compare
end)
module Seen = Set.Make(struct
  type t = int
  let compare = compare
end)

let process_line line =
  if Str.string_match re_full line 0 then
    let id = int_of_string (Str.matched_group 1 line) in
    let connections = List.map int_of_string (Str.split_delim (Str.regexp ", ") (Str.matched_group 2 line)) in
    id, connections
  else
    failwith ("can't parse: " ^ line)


let process_graph lines =
  let rec aux g = function
    | [] -> g
    | x :: xs ->
      let id, connections = process_line x in
      aux (Graph.add id connections g) xs in
  aux Graph.empty lines


let find_connected g root =
  let rec aux el seen =
    if Seen.mem el seen then
      seen
    else
      let seen' = Seen.add el seen in
      let children = Graph.find el g in
      List.fold_right aux children seen' in
  aux root Seen.empty


let () =
  let raw_lines = List.map String.strip (Std.input_list (open_in file)) in
  let g = process_graph raw_lines in
  Graph.iter (fun k v -> printf "%d => %s\n" k (String.join "," (List.map string_of_int v))) g ;
  let s = find_connected g 0 in
  printf "answer = %d\n" (Seen.cardinal s)
