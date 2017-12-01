(* day 01, part 1 *)

open Printf
open ExtString

let file = "input.dat"

let part_1 aa =
  let exploded_input = String.explode aa in
  let raw_digits = List.map (fun x -> Char.code x - 0x30) exploded_input in
  let digits = (List.hd raw_digits) :: raw_digits in
  let rec aux acc = function
    | [] -> acc
    | [a] -> acc
    | a :: (b :: _ as xs) ->
      if a = b then aux (acc + a) xs else aux acc xs in
  aux 0 digits

(* day 01, part 2 *)

(*
this time we actually want direct access to any element in
the "list" of numbers.. so lets use an array instead
*)
let part_2 aa =
  let exploded_input = Array.of_list (String.explode aa) in
  let digits = Array.map (fun x -> Char.code x - 0x30) exploded_input in
  let num_digits = Array.length digits in
  let idx_modifier = (num_digits / 2) in
  let rec aux idx acc lst =
    let match_digit = Array.get digits ((idx + idx_modifier) mod num_digits) in
    match lst with
    | [] -> acc
    | [b] -> if b = match_digit then acc + b else acc
    | a :: xs -> if a = match_digit then aux (idx + 1) (acc + a) xs else aux (idx + 1) acc xs in
  aux 0 0 (Array.to_list digits)

let () =
  let raw_input = String.strip (Std.input_file file) in
  print_endline ("part 1: " ^ (string_of_int (part_1 raw_input))) ;
  print_endline ("part 2: " ^ (string_of_int (part_2 raw_input)))
