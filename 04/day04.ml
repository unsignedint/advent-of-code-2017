(* day 04 *)

open Printf
open ExtString

let file = "input.txt"

let part_1_input = "aa bb cc dd ee aa"

module SS = Set.Make(String)

let part_1 lines =
  let valid_passphrase phrase =
    let words = List.map String.strip (Str.split (Str.regexp " ") phrase) in
    let the_set = List.fold_right SS.add words SS.empty in
    SS.cardinal the_set = List.length words in
  List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 (List.map valid_passphrase lines)

let () =
  let raw_input = List.map String.strip (Std.input_list (open_in file)) in
  print_endline ("part 1: " ^ (string_of_int (part_1 raw_input))) ;
