(* day 11 *)

open Printf
open ExtString

let file = "input.txt"
(* let test_input = "se,sw,se,sw,sw" *)


let hex_dir_to_2d_coord = function
  | "n"  -> (-1, 1)
  | "ne" -> ( 0, 1)
  | "nw" -> (-1, 0)
  | "se" -> ( 1, 0)
  | "sw" -> ( 0,-1)
  | "s"  -> ( 1,-1)
  | _    -> failwith "wtf?"


let calculate_distance x y =
  let x, y = abs x, abs y in
  let a, b = max x y, min x y in
  (a - b) + b


let rec process x y = function
  | [] -> (x, y)
  | el :: tail ->
    let xx, yy = hex_dir_to_2d_coord el in
    process (x+xx) (y+yy) tail


let () =
  let aaa = String.strip (Std.input_file file) in
  let raw_input = Str.split (Str.regexp ",") aaa in
  let x, y = process 0 0 raw_input in
  let ans = calculate_distance x y in
  printf "x=%d y=%d\n" x y ;
  printf "answer=%d\n" ans
