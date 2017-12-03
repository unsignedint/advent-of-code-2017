(* day 03 *)

open Printf
open ExtString

let part_1_input = 277678

let part_1 v =
  let width = float_of_int v |> sqrt |> ceil |> int_of_float in
  let offset = v mod width in
  let centre_ceil = (float_of_int width) /. 2. |> ceil |> int_of_float in
  let centre_floor = width / 2 in
  (offset - centre_ceil) + centre_floor

let () =
  print_endline ("part 1: " ^ (string_of_int (part_1 part_1_input))) ;
