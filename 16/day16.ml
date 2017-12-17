(* day 16 *)

open Printf
open ExtString

(* let test_data = "s1,x3/4,pe/b" *)
(* let num_programs = 5 *)
let test_data = String.strip (Std.input_file "input.txt")
let num_programs = 16

let gen_programs n =
  let a = Array.make n 'a' in
  let rec aux x =
    if x = n then ()
    else (a.(x) <- (Char.chr (Char.code 'a' + x)) ; aux (x + 1)) in
  aux 0 ; a

type instruction = Spin of int | Exchange of int * int | Partner of char * char

let gen_instructions lst =
  let process instr =
    let instr_len = String.length instr in
    let instr_operand = String.sub instr 1 (instr_len-1) in
    match instr.[0] with
      | 's' -> Spin (int_of_string (String.sub instr 1 (instr_len-1)))
      | 'x' ->
        let operands = List.map int_of_string (String.split_on_char '/' instr_operand) in
        Exchange (List.hd operands, List.hd (List.tl operands))
      | 'p' ->
        let operands = List.map (fun x -> x.[0]) (String.split_on_char '/' instr_operand) in
        Partner (List.hd operands, List.hd (List.tl operands))
      | _ -> failwith "unknown?" in
  List.map process lst

let print_instr = function
  | Spin n -> printf "spin %d\n" n
  | Exchange (x, y) -> printf "exchange %d %d\n" x y
  | Partner (a, b) -> printf "partner %c %c\n" a b

let find_index a c =
  let rec aux idx = if a.(idx) = c then idx else aux (idx+1) in
  aux 0

let process lst state =
  let update s = function
    | Spin n ->
      let len = Array.length s in
      let first_piece = Array.sub s (len - n) (len - (len - n)) in
      let second_piece = Array.sub s 0 (len - n) in
      Array.append first_piece second_piece
    | Exchange (x, y) ->
      let xx = s.(x) in (s.(x) <- s.(y) ; s.(y) <- xx ; s)
    | Partner (a, b) ->
      let x = find_index s a in
      let y = find_index s b in
      let xx = s.(x) in ( s.(x) <- s.(y) ; s.(y) <- xx ; s) in
  List.fold_left update state lst

let find_period f s =
  let initial_state = Array.copy s in
  let rec aux state n =
    let s' = f state in
    if s' = initial_state then n
    else aux s' (n + 1) in
  aux s 1

let rec repeat f s n =
  let s' = f s in
  if n = 1 then s'
  else repeat f s' (n-1)

(*
for part #2 we rely on the cycle's period to avoid having to do the actual
one billion rounds... we just repeat the number of times since the last
complete cycle
*)
let () =
  let s = gen_programs num_programs in
  let original_s = Array.copy s in
  let instructions = Str.split_delim (Str.regexp ",") test_data |> gen_instructions in
  (* List.iter print_instr instructions ; *)
  (* print_endline ("start = " ^ (String.join "" (List.map Char.escaped (Array.to_list s)))) ; *)
  let s' = process instructions s in
  print_endline (String.join "" (List.map Char.escaped (Array.to_list s'))) ;
  let period = find_period (process instructions) (Array.copy original_s) in
  printf "start = %s\n" (String.join "" (List.map Char.escaped (Array.to_list original_s))) ;
  printf "period = %d\n" period ;
  let remaining_iterations = 1_000_000_000 mod period in
  printf "remaining_iterations = %d\n" remaining_iterations ;
  let s'' = repeat (process instructions) (Array.copy original_s) (remaining_iterations) in
  print_endline (String.join "" (List.map Char.escaped (Array.to_list s''))) ;
