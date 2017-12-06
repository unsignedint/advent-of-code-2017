(* day 05 *)

open Printf
open ExtString

let file = "input.txt"


let part_1 values =
  let a = Array.of_list values in
  let upper_bound = Array.length a in
    (* printf "upper_bound: %d, array: %s\n" upper_bound (String.join "," (List.map string_of_int (Array.to_list a))) ; *)
    let rec aux moves idx =
    (* printf "moves: %d, idx: %d, array: %s\n" moves idx (String.join "," (List.map string_of_int (Array.to_list a))) ; *)
      if idx < 0 || idx >= upper_bound then
        moves
      else
        let curr = Array.get a idx in
        let next_idx = idx + curr in
        Array.set a idx (curr + 1) ;
        aux (moves + 1) next_idx in
  aux 0 0
  (* printf "%s array: %s\n" e (String.join "," (List.map string_of_int (Array.to_list a))) ; *)


let part_2 values =
  let a = Array.of_list values in
  let upper_bound = Array.length a in
    (* printf "upper_bound: %d, array: %s\n" upper_bound (String.join "," (List.map string_of_int (Array.to_list a))) ; *)
    let rec aux moves idx =
    (* printf "moves: %d, idx: %d, array: %s\n" moves idx (String.join "," (List.map string_of_int (Array.to_list a))) ; *)
      if idx < 0 || idx >= upper_bound then
        moves
      else
        let curr = Array.get a idx in
        let next_idx = idx + curr in
        if curr >= 3 then
          Array.set a idx (curr - 1)
        else
          Array.set a idx (curr + 1) ;
        aux (moves + 1) next_idx in
  aux 0 0
  (* printf "%s array: %s\n" e (String.join "," (List.map string_of_int (Array.to_list a))) ; *)

let () =
  let raw_input = List.map String.strip (Std.input_list (open_in file)) in
  let values = List.map int_of_string raw_input in
  print_endline ("part 1: " ^ (string_of_int (part_1 values))) ;
  print_endline ("part 2: " ^ (string_of_int (part_2 values))) ;
  (* print_endline ("part 2: " ^ (string_of_int (part_2 raw_input))) ; *)
