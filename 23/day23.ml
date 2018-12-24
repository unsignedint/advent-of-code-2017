(* day 23 *)

open Printf
open ExtString

let test_data = List.map String.strip (Std.input_list (open_in "input.txt"))

module Registers = Map.Make(String)

type instruction =
  Set of (string * string) | Sub of (string * string) | Mul of (string * string) | Jnz of (string * string)

(* type action = Block | Step of int *)

let print_instruction = function
  | Set (x, n) -> sprintf "set(%s)=>%s" x n
  | Sub (x, n) -> sprintf "sub(%s)=>%s" x n
  | Mul (x, y) -> sprintf "mul(%s)=>%s" x y
  | Jnz (x, y) -> sprintf "jnz(%s)=>%s" x y


let print_instruction2 i = function
  | Set (x, n) -> sprintf "%s = %s" x n
  | Sub (x, n) -> sprintf "%s -= %s" x n
  | Mul (x, y) -> sprintf "%s *= %s" x y
  | Jnz (x, y) -> sprintf "if (%s != 0) goto L%02d" x (i + (int_of_string y))


let gen_instructions lst =
  let process instr =
    let pieces = String.split_on_char ' ' instr in
    match pieces with
      | "set" :: x :: n :: [] -> Set (x, n)
      | "sub" :: x :: n :: [] -> Sub (x, n)
      | "mul" :: x :: y :: [] -> Mul (x, y)
      | "jnz" :: x :: y :: [] -> Jnz (x, y)
      | _ -> failwith "unknown?" in
  List.map process lst

let reg_get_or_zero key m =
  match Registers.find_opt key m with
    | Some v -> v
    | None -> if key = "a" then 1 else 0

let process_instruction instr r mul_count =
  let get_val_or_reg unk r =
    try int_of_string unk
    with Failure _ -> reg_get_or_zero unk r in

  match instr with
    | Set (x, n) -> 1, Registers.add x (get_val_or_reg n r) r, mul_count
    | Sub (x, n) -> let curr = get_val_or_reg x r in 1, Registers.add x (curr - (get_val_or_reg n r)) r, mul_count
    | Mul (x, y) -> let curr = get_val_or_reg x r in 1, Registers.add x (curr * (get_val_or_reg y r)) r, (mul_count + 1)
    | Jnz (x, y) -> if (get_val_or_reg x r) <> 0 then (get_val_or_reg y r, r, mul_count) else (1, r, mul_count)


let executor instructions =
  let rec execute idx r mul_count =
    try
      let instruction = instructions.(idx) in
      let step, r', send_count' = process_instruction instruction r mul_count in
      execute (idx + step) r' send_count'
    with Invalid_argument _ | Stack_overflow ->
      printf "Abort at %d => %s\n" idx (print_instruction instructions.(idx)) ;
      idx, r, mul_count in
  execute 0 Registers.empty 0

let while_executor instructions =
  let quit_loop = ref false in
  (* let loop_count = ref 0 in *)
  let idx = ref 0 in
  let r = ref Registers.empty in
  let mul_count = ref 0 in
  while not !quit_loop do
    try
      let instruction = instructions.(!idx) in
      let step, r', mul_count' = process_instruction instruction !r !mul_count in
      begin
        idx := !idx + step ;
        r := r' ;
        mul_count := mul_count' ;
        (* if !loop_count > 1000000 then *)
          (* quit_loop := true *)
        (* else *)
          (* loop_count := !loop_count + 1 *)
      end
    with Invalid_argument _ ->
        quit_loop := true
      | Stack_overflow ->
        printf "Abort at %d => %s\n" !idx (print_instruction instructions.(!idx)) ;
        quit_loop := true
  done ;
  !idx, !r, !mul_count

let () =
  let instructions = gen_instructions test_data |> Array.of_list in
  List.iteri (fun i x -> printf "L%02d: %s\n" i (print_instruction2 i x)) (Array.to_list instructions)
  (* let _, r, mul_count = while_executor instructions in
  printf "mul_count = %d\n" mul_count ;
  Registers.iter (fun k v -> printf "%s=%d\n" k v) r *)
