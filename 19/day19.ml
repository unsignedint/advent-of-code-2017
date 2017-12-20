(* day 19 *)

open Printf
open ExtString

(* original board as 2d matrix *)
let test_data = List.map (fun x -> String.explode x |> Array.of_list) (Std.input_list (open_in "input.txt"))

type matrix_data = {
  num_rows: int;
  num_cols: int;
  data: char array array;
}
type direction = Down | Up | Left | Right

let create_matrix lst =
  let num_cols = Array.length (List.hd lst) in
  let num_rows = List.length lst in
  let matrix = Array.make_matrix num_cols num_rows ' ' in
  let rec aux row_idx = function
    | [] -> {num_rows=num_rows; num_cols=num_cols; data=matrix}
    | x :: xs -> begin
      for j=0 to num_cols-1 do
        matrix.(j).(row_idx) <- x.(j)
      done ;
      aux (row_idx + 1) xs
      end in
  aux 0 lst

let find_start matrix =
  let data = matrix.data in
  let rec aux col =
    if data.(col).(0) = '|' then col, 0
    else aux (col + 1) in
  aux 0

let print_matrix matrix =
  for i=0 to matrix.num_rows-1 do
    for j=0 to matrix.num_cols-1 do
      printf "%c" matrix.data.(j).(i)
    done ;
    print_endline ""
  done

let follow matrix start_x start_y =
  let rec aux acc nsteps dir x y =
    let nsteps = nsteps + 1 in
    match matrix.data.(x).(y) with
      | '|' | '-' ->
      begin
        match dir with
          | Down -> aux acc nsteps dir x (y+1)
          | Up -> aux acc nsteps dir x (y-1)
          | Left -> aux acc nsteps dir (x-1) y
          | Right -> aux acc nsteps dir (x+1) y
      end
      | 'A'..'Z' as letter ->
      begin
        printf "met %c\n" letter ;
        let acc = (Char.escaped letter) :: acc in
        match dir with
          | Down -> aux acc nsteps dir x (y+1)
          | Up -> aux acc nsteps dir x (y-1)
          | Left -> aux acc nsteps dir (x-1) y
          | Right -> aux acc nsteps dir (x+1) y
      end
      | '+' ->
      begin
        let c_down = matrix.data.(x).(y+1) in
        (* let c_up = matrix.data.(x).(y-1) in *)
        let c_left = matrix.data.(x-1).(y) in
        (* let c_right = matrix.data.(x+1).(y) in *)
        match dir with
          | Down | Up -> if c_left = '-' then aux acc nsteps Left (x-1) y else aux acc nsteps Right (x+1) y
          | Left | Right -> if c_down = '|' then aux acc nsteps Down x (y+1) else aux acc nsteps Up x (y-1)
      end
      | ' ' -> List.rev acc, (nsteps - 1)
      | _ as c -> failwith (sprintf "what? %c" c) in
    aux [] 0 Down start_x start_y


let () =
  let matrix = create_matrix test_data in
  (* print_matrix matrix *)
  let start_x, start_y = find_start matrix in
  printf "start: %d,%d\n" start_x start_y ;
  let letters, nsteps = follow matrix start_x start_y in
  printf "answer = %s, nsteps = %d\n" (String.join "" letters) nsteps
