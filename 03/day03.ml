(* day 03 *)

open Printf
open ExtString

let part_1_input = 20
let part_2_input = 5

let part_1 v =
  let width = float_of_int v |> sqrt |> ceil |> int_of_float in
  let offset = width - (v mod width) in
  let centre_floor = width / 2 in
  abs (offset - centre_floor) + centre_floor

type coord = {x : int; y : int}
module Board = Map.Make(struct
  type t = coord
  let compare = compare
end)

let neighbors {x;y} = [
  x,  y+1;
  x+1,y+1;
  x+1,y;
  x+1,y-1;
  x,  y-1;
  x-1,y-1;
  x-1,y;
  x-1,y+1;
] |> List.map (fun (x,y) -> {x;y})

let fold_neighbors board cell ~f ~init =
  neighbors cell |>
  List.fold_left (fun acc n ->
      try f acc (Board.find n board)
      with Not_found -> acc)
    init

let the_board = Board.empty

type direction = North | West | South | East

let string_of_direction = function
  | North -> "north"
  | West -> "west"
  | South -> "south"
  | East -> "east"

let part_2 v =
  let rec aux board m n dir x y z =
    print_endline ("move: " ^ (string_of_direction dir) ^ " step: " ^ (string_of_int n)
                   ^ " x: " ^ (string_of_int x) ^ " y: " ^ (string_of_int y) ^ " z: " ^ (string_of_int z)) ;
    let board = Board.add {x=x; y=y} z board in
    if m = v then
      board
    else
      if n < m then
        (* continue in the same direction *)
        let xx, yy = match dir with
          | East -> x+1, y
          | North -> x, y+1
          | West -> x-1, y
          | South -> x, y-1 in
        aux board m (n + 1) dir xx yy (z+1)
      else
        (* change direction *)
        match dir with
          | East -> aux board m 1 North x (y+1) (z+1)
          | North -> aux board (m + 1) 1 West (x-1) y (z+1)
          | West -> aux board m 1 South x (y-1) (z+1)
          | South -> aux board (m + 1) 1 East (x+1) y (z+1)
  in
  aux Board.empty 1 0 East 0 0 1

let print_piece k v =
  print_endline ("x=" ^ (string_of_int k.x) ^ ";" ^ "y=" ^ (string_of_int k.y) ^ " " ^ (string_of_int v))

let () =
  print_endline ("part 1: " ^ (string_of_int (part_1 part_1_input))) ;
  let board = part_2 part_2_input in
  Board.iter print_piece board ;
  (* print_endline ("part 2: " ^ (string_of_int (part_2 part_2_input))) ; *)
