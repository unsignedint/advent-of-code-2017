(* day 06 *)

open Printf
open ExtString

(* let part_1_input = [0; 2; 7; 0] *)
let part_1_input = [11;11;13;7;0;15;5;5;4;4;1;1;7;1;15;11]

module SL = Set.Make(struct
  type t = int array
  let compare = compare
end)


let part_1 values =
  let a = Array.of_list values in
  let mod_inc idx = (idx + 1) mod (Array.length a) in
  let rec aux moves s =
    printf "moves: %d, array: %s\n" moves (String.join "," (List.map string_of_int (Array.to_list a))) ;
    if SL.mem a s then
      moves
    else
      let s = SL.add (Array.copy a) s in
      (* printf "moves: %d, idx: %d, array: %s\n" moves idx (String.join "," (List.map string_of_int (Array.to_list a))) ; *)
      (* let s = SL.add a s in *)
      let max_val, max_idx, _ = Array.fold_left (fun (old_max, old_idx, curr_idx) x ->
        let new_max = max old_max x in
        if new_max > old_max then
          new_max, curr_idx, curr_idx + 1
        else
          old_max, old_idx, curr_idx + 1) (0, 0, 0) a in
      let rec redistribute acc idx =
        Array.set a idx ((Array.get a idx) + 1) ;
        if acc = 1 then ()
        else redistribute (acc - 1) (mod_inc idx) in
      printf "max_val: %d, max_idx: %d\n" max_val max_idx ;
      Array.set a max_idx 0 ;
      redistribute max_val (mod_inc max_idx) ;
      aux (moves + 1) s in
  aux 0 SL.empty
  (* printf "%s array: %s\n" e (String.join "," (List.map string_of_int (Array.to_list a))) ; *)


let () =
  (* let raw_input = List.map String.strip (Std.input_list (open_in file)) in
  let values = List.map int_of_string raw_input in *)
  print_endline ("part 1: " ^ (string_of_int (part_1 part_1_input))) ;
  (* print_endline ("part 2: " ^ (string_of_int (part_2 values))) ; *)
  (* print_endline ("part 2: " ^ (string_of_int (part_2 raw_input))) ; *)
