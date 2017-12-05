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
  let rec aux m n dir =
    print_endline ("moving: " ^ (string_of_direction dir) ^ " step: " ^ (string_of_int n)) ;
    if m = v then
      5
    else if n < m then
      aux m (n + 1) dir
    else
      (* change direction *)
      match dir with
        | East -> aux m 1 North
        | North -> aux (m + 1) 1 West
        | West -> aux m 1 South
        | South -> aux (m + 1) 1 East
  in
  aux 1 1 East

let () =
  print_endline ("part 1: " ^ (string_of_int (part_1 part_1_input))) ;
  print_endline ("part 2: " ^ (string_of_int (part_2 part_2_input))) ;
