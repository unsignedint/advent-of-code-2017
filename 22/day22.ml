(* day 22 *)

open Printf
open ExtString

let test_data = List.map String.strip (Std.input_list (open_in "input.txt"))

type node_state = Clean | Infected
type direction = North | South | West | East

type 'a mat = { n: int; m: int; t: 'a array array }
let create_mat n m e = { n=n; m=m; t = Array.make_matrix n m e }

type coord = {x : int; y : int}
module Board = Map.Make(struct
  type t = coord
  let compare = compare
end)

let turn_left = function
  | North -> West
  | West -> South
  | South -> East
  | East -> North

let turn_right = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let move_forward pos = function
  | North -> {x=pos.x; y=pos.y-1}
  | South -> {x=pos.x; y=pos.y+1}
  | West -> {x=pos.x-1; y=pos.y}
  | East -> {x=pos.x+1; y=pos.y}

let fold_lefti_on_mat f acc m =
  let acc' = ref acc in
  for i = 0 to m.n-1 do
    for j = 0 to m.m-1 do
      acc' := f !acc' m.t.(i).(j) i j
    done
  done ; !acc'

let fst_mat m = m.t.(0).(0)

let apply_mat m f =
  let m' = create_mat m.n m.m (fst_mat m) in
  for i = 0 to m.n-1 do
    for j = 0 to m.m-1 do
      f m' i j
    done
  done ; m'

let lines_to_mat slist =
  let t = Array.of_list (List.map (fun x -> String.explode x |> Array.of_list) slist) in
  let n = Array.length t.(0) in
  let m = Array.length t in
  {n; m; t}


let build_map matrix board =
  let offset = {x=matrix.n/2; y=matrix.m/2} in
  let add_node_fun acc node i j =
    (* remember i = rows, j = columns *)
    let pos = {x=j-offset.x;y=i-offset.y} in
    if node = '#' then Board.add pos Infected acc
    else acc in
  fold_lefti_on_mat add_node_fun board matrix

let perform_round acc =
  let board, pos, dir, infection_count = acc in
  let piece_state = match Board.find_opt pos board with
    | Some v -> v
    | None -> Clean in
  let dir' = match piece_state with
    | Clean -> turn_left dir
    | Infected -> turn_right dir in
  let board' = match piece_state with
    | Clean -> Board.add pos Infected board
    | Infected -> Board.add pos Clean board in
  let pos' = move_forward pos dir' in
  let infection_count' = if piece_state = Clean then infection_count + 1 else infection_count in
  board', pos', dir', infection_count'

let better_repeat f a n =
  let rec aux acc n =
    if n = 0 then acc
    else aux (f acc) (n-1)
  in aux a n

let print_mat m =
  for i=0 to m.n-1 do
    (* printf "%02d: " i ; *)
    for j=0 to m.m-1 do
      printf "%c" m.t.(i).(j)
    done ;
    printf "\n"
  done ; printf "\n"

let board_to_mat board =
  let m = create_mat 20 20 '.' in
  apply_mat m (fun m' i j ->
    try m'.t.(i).(j) <- (if Board.find {x=j-10;y=i-10} board = Infected then '#' else '.')
    with Not_found -> ())

let () =
  let m = lines_to_mat test_data in
  let board = build_map m Board.empty in
  (* printf "offset=%d,%d\n" result.x result.y *)
  (* Board.iter (fun k _ -> printf "x=%d y=%d\n" k.x k.y) result *)
  let board', _, _, c = better_repeat perform_round (board, {x=0;y=0}, North, 0) 10000 in
  (* (Board.iter (fun k v -> printf "x=%d y=%d %s\n" k.x k.y (if v = Clean then "CLEAN" else "INFECTED")) board' ; *)
  (* print_mat (board_to_mat board') ; *)
  printf "infection count = %d\n" c
