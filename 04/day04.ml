(* day 04 *)

open Printf
open ExtString

let file = "input.txt"

module SS = Set.Make(String)

let part_1 lines =
  let valid_passphrase phrase =
    let words = List.map String.strip (Str.split (Str.regexp " ") phrase) in
    let the_set = List.fold_right SS.add words SS.empty in
    SS.cardinal the_set = List.length words in
  List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 (List.map valid_passphrase lines)

module SC = Set.Make(Char)
module SSC = Set.Make(SC)

let part_2 lines =
  let valid_passphrase phrase =
    let words = List.map String.strip (Str.split (Str.regexp " ") phrase) in
    let make_word_set word = List.fold_right SC.add (String.explode word) SC.empty in
    let the_set = List.fold_right SSC.add (List.map make_word_set words) SSC.empty in
    SSC.cardinal the_set = List.length words in
  List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 (List.map valid_passphrase lines)

let () =
  let raw_input = List.map String.strip (Std.input_list (open_in file)) in
  print_endline ("part 1: " ^ (string_of_int (part_1 raw_input))) ;
  print_endline ("part 2: " ^ (string_of_int (part_2 raw_input))) ;
