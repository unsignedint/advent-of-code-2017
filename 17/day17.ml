(* day 17 *)

open Printf
open ExtString

let step = 343

let mod_counter a start_idx =
  (start_idx + step) mod (Array.length a)

let array_insert a n x =
  let first_piece = Array.sub a 0 n in
  let second_piece = Array.sub a n (Array.length a - n) in
  Array.concat [first_piece; [| x |]; second_piece]

let update a n idx =
  let idx' = mod_counter a idx in
  array_insert a (idx' + 1) n, idx' + 1

let repeat f a =
  let rec aux a idx n round =
    (* printf "idx = %d, n = %d, result = %s\n" idx n (String.join " " (List.map string_of_int (Array.to_list a))) ; *)
    let a', idx' = f a n idx in
    if round = 1 then a', idx'
    else aux a' idx' (n + 1) (round - 1) in
  aux a 0 1 2017

let part2_update n idx =
  (idx + step) mod n + 1

let part2_repeat f rounds =
  let rec aux zero_val idx n round =
    (* printf "idx = %d, n = %d\n" idx n ; *)
    let idx' = f n idx in
    let zero_val' = if idx' = 1 then n else zero_val in
    if round = 1 then idx', zero_val'
    else aux zero_val' idx' (n + 1) (round - 1) in
  aux 0 0 1 rounds

let () =
  let a = [| 0 |] in
  let a', idx' = repeat update a in
  printf "idx = %d, result = %s\n" idx' (String.join " " (List.map string_of_int (Array.to_list a'))) ;
  let idx', zero_val = part2_repeat part2_update 50_000_000 in
  printf "idx2 = %d, zero_val = %d\n" idx' zero_val ;
