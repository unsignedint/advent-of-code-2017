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
    Graph.add depth range g in
  List.fold_right add_to_graph lines Graph.empty

let update_state g i v =
  if Graph.mem i g = false then 1, Down
  else
    let range = Graph.find i g in
    let n, direction = v in
    match direction with
      | Down -> if n = range-1 then n-1, Up else n+1, Down
      | Up -> if n = 0 then n+1, Down else n-1, Up

let find_solution g =
  let max_depth = (Graph.fold (fun k _ acc -> max k acc) g 0) + 1 in
  let rec aux s n acc =
    if n = max_depth then
      acc
    else
      (* update position *)
      let state' = Array.mapi (update_state g) s in
      printf "state (%d) = %s\n" n (String.join "," (Array.to_list (Array.map (fun x -> fst x |> string_of_int) state'))) ;
      let v, _ = Array.get state' n in
      if v = 0 then
        aux state' (n + 1) (n :: acc)
      else
        aux state' (n + 1) acc in
  aux (Array.make max_depth (-1, Down)) 0 []


let () =
  let raw_lines = List.map String.strip (Std.input_list (open_in file)) in
  let g = build_network raw_lines in
  Graph.iter (fun k v -> printf "%d => %d\n" k v) g ;
  let sol = find_solution g in
  printf "solution = %s\n" (String.join "," (List.map string_of_int sol)) ;
  (* List.iter (printf "%d,") sol ; *)
  printf "answer = %d\n" (List.fold_left (fun acc x -> acc + x * Graph.find x g) 0 sol) ;

