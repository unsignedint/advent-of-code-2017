(* day 24 *)

open Printf
open ExtString

let test_data = List.map String.strip (Std.input_list (open_in "input.txt"))

type piece = {
  ports: int * int;
  strength: int;
}
module PS = Set.Make(struct
  type t = piece
  let compare = compare
end)

let string_of_piece piece =
  sprintf "%d<>%d (%d)" (fst piece.ports) (snd piece.ports) piece.strength

let process_input lines =
  List.map (fun l ->
    let pieces = String.split_on_char '/' l in
    let ports = (int_of_string (List.hd pieces), int_of_string (List.nth pieces 1)) in
    {ports=ports; strength=(fst ports + snd ports)}
  ) lines

let filter_pieces pieces port =
  PS.filter (fun e -> (fst e.ports = port) || (snd e.ports = port)) pieces

let get_other_port piece port =
  match piece.ports with
    | (a, b) when a = port -> b
    | (a, b) when b = port -> a
    | _ -> failwith "unmatched port"

let build_bridge_part1 pieces =
  let rec aux port pieces strength =
    let valid_pieces = filter_pieces pieces port in
    if PS.cardinal valid_pieces = 0 then
      strength (* no matching pieces to continue with *)
    else
      let choices = List.map (fun p -> aux (get_other_port p port) (PS.remove p pieces) (strength + p.strength)) (PS.elements valid_pieces) in
      List.fold_left (fun acc x -> if x > acc then x else acc) (List.hd choices) choices
  in
  aux 0 pieces 0

let build_bridge_part2 pieces =
  let rec aux port pieces length strength =
    let valid_pieces = filter_pieces pieces port in
    if PS.cardinal valid_pieces = 0 then
      length, strength (* no matching pieces to continue with *)
    else
      let choices = List.map (fun p -> aux (get_other_port p port) (PS.remove p pieces) (length + 1) (strength + p.strength)) (PS.elements valid_pieces) in
      List.fold_left (fun acc x ->
        let acc_len, acc_strength = acc in
        let x_len, x_strength = x in
        if x_len > acc_len then x
        else if x_len = acc_len && x_strength > acc_strength then x
        else acc
      ) (List.hd choices) choices
  in
  snd (aux 0 pieces 0 0)

let () =
  let pieces = List.fold_right PS.add (process_input test_data) PS.empty in
  (* let start = filter_pieces pieces 10 in
  PS.iter (fun x -> print_endline (string_of_piece x)) start *)
  let max_strength1 = build_bridge_part1 pieces in
  let max_strength2 = build_bridge_part2 pieces in
  printf "answer1=%d\n" max_strength1 ;
  printf "answer2=%d\n" max_strength2
