(* day 02 *)

open Printf
open ExtString

let file = "input.dat"

let line_to_list_of_ints line =
  let split_vals = List.map String.strip (Str.split (Str.regexp "\t+") line) in
  (* print_endline ("zzz: " ^ (String.concat "#" split_vals)); *)
  List.map int_of_string split_vals

let part_1 lines =
  let calc_of_line line =
    let values = line_to_list_of_ints line in
    let max_value = List.fold_left (fun acc x -> max acc x) 0 values in
    let min_value = List.fold_left (fun acc x -> min acc x) max_int values in
    (* print_endline ("max: " ^ (string_of_int max_value)); *)
    (* print_endline ("min: " ^ (string_of_int min_value)); *)
    max_value - min_value in
  List.fold_left (fun acc x -> acc + x) 0 (List.map calc_of_line lines)


let part_2 lines =
  let find_divisible_numbers line =
    let values = line_to_list_of_ints line in
    let ascending = List.sort compare values in
    let descending = List.rev ascending in
    let predicate a b = a != b && a mod b = 0 in
    let checker v =
      let exists = List.exists (predicate v) ascending in
      if exists then
        let z = List.find (predicate v) ascending in
        (* print_endline ("v: " ^ (string_of_int v)); *)
        (* print_endline ("z: " ^ (string_of_int z)); *)
        v / z
      else 0 in
    let rec aux = function
      | [] -> failwith "boom"
      | x :: xs ->
        let result = checker x in
        if result = 0 then aux xs
        else result in
    aux descending in
  List.fold_left (fun acc x -> acc + x) 0 (List.map find_divisible_numbers lines)

let () =
  let raw_input = List.map String.strip (Std.input_list (open_in file)) in
  print_endline ("part 1: " ^ (string_of_int (part_1 raw_input))) ;
  print_endline ("part 2: " ^ (string_of_int (part_2 raw_input))) ;
