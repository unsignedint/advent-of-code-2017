(* day 09 *)

open Printf
open ExtString
(*              123  3456     3                *)
(* let test_str = "{{{},{{{{}}}},{}}}" *)
let file = "input.txt"


type char_state = Inbetween of int | Group of int | Garbage | Escape

let str_of_state = function
  | Inbetween x -> sprintf "Inbetween(%d)" x
  | Group x -> sprintf "Group(%d)" x
  | Garbage -> "Garbage"
  | Escape -> "Escape"


(*
process each character and use a "state stack" to know how to treat the next character
recursively find all groups, dfs post-order, return depth which is accumulated to make the score
*)

let rec check_char count g state remaining =
  let s = List.hd state in
  let xs = List.tl state in
  match remaining with
  | [] -> xs, g
  | c :: _ ->
    (* printf "c = %c (%d) state = %s, => %s\n" c count (str_of_state s) (String.join ", " (List.map str_of_state xs)); *)
    let c = List.hd remaining in
    let next_state = match s with
    | Inbetween x -> begin
      match c with
        | '{' -> Group (x+1) :: xs
        | ',' -> state
        | '}' -> Inbetween (x-1) :: xs
        | '<' -> Garbage :: state
        | _ -> failwith (sprintf "what is %c?" c)
      end
    | Group x -> begin
      match c with
        | '{' -> Group (x+1) :: state
        | '}' -> Inbetween (x-1) :: state
        | '<' -> Garbage :: state
        | _ -> state
      end
    | Garbage -> begin
      match c with
        | '>' -> xs  (* no more garbage *)
        | '!' -> Escape :: state
        | _ -> state
      end
    | Escape -> xs in
    let next_g = match s with
    | Garbage -> begin
      match c with
      | '>' -> g
      | '!' -> g
      | _ -> g + 1
    end
    | _ -> g in
  check_char (count+1) next_g next_state (List.tl remaining)


let process_input chars =
  let states, garbage = check_char 1 0 [Inbetween 0] chars in
  print_endline (String.join ", " (List.map str_of_state states)) ;
  print_endline (string_of_int (List.fold_left (fun acc a ->
    match a with
      | Group x -> acc + x
      | _ -> acc ) 0 states) ; ) ;
  printf "garbage: %d\n" garbage


let () =
  let raw_input = String.strip (Std.input_file file) in
  let exploded_input = String.explode raw_input in
  (* let exploded_input = String.explode test_str in *)
  process_input exploded_input
