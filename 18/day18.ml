(* day 18 *)

open Printf
open ExtString

let test_data = List.map String.strip (Std.input_list (open_in "input.txt"))

module Registers = Map.Make(String)

type instruction =
  Snd of (string * string) | Set of (string * string) | Add of (string * string) | Mul of (string * string) |
  Mod of (string * string) | Rcv of (string * string) | Jgz of (string * string)

type action = Block | Step of int

let print_instruction = function
  | Snd (x, _) -> sprintf "snd(%s)" x
  | Set (x, n) -> sprintf "set(%s)=>%s" x n
  | Add (x, n) -> sprintf "add(%s)=>%s" x n
  | Mul (x, y) -> sprintf "mul(%s)=>%s" x y
  | Mod (x, n) -> sprintf "mod(%s)=>%s" x n
  | Rcv (x, _) -> sprintf "rcv(%s)" x
  | Jgz (x, n) -> sprintf "jgz(%s)=>%s" x n


let gen_instructions lst =
  let process instr =
    let pieces = String.split_on_char ' ' instr in
    match pieces with
      | "snd" :: x :: [] -> Snd (x, "nil")
      | "set" :: x :: n :: [] -> Set (x, n)
      | "add" :: x :: n :: [] -> Add (x, n)
      | "mul" :: x :: y :: [] -> Mul (x, y)
      | "mod" :: x :: n :: [] -> Mod (x, n)
      | "rcv" :: x :: [] -> Rcv (x, "nil")
      | "jgz" :: x :: n :: [] -> Jgz (x, n)
      | _ -> failwith "unknown?" in
  List.map process lst

let reg_get_or_zero prog_id key m =
  match Registers.find_opt key m with
    | Some v -> v
    | None -> (* printf "prog %d\n" prog_id ; *) if key = "p" then prog_id else 0

let process_instruction prog_id instr out_q in_q r send_count =
  let get_val_or_reg unk r =
    try int_of_string unk
    with Failure _ -> reg_get_or_zero prog_id unk r in

  match instr with
    | Snd (x, _) -> Step 1, out_q @ [(get_val_or_reg x r)], in_q, r, (send_count + 1)
    | Set (x, n) -> Step 1, out_q, in_q, Registers.add x (get_val_or_reg n r) r, send_count
    | Add (x, n) -> let curr = get_val_or_reg x r in Step 1, out_q, in_q, Registers.add x (curr + (get_val_or_reg n r)) r, send_count
    | Mul (x, y) -> let curr = get_val_or_reg x r in Step 1, out_q, in_q, Registers.add x (curr * (get_val_or_reg y r)) r, send_count
    | Mod (x, n) -> let curr = get_val_or_reg x r in Step 1, out_q, in_q, Registers.add x (curr mod (get_val_or_reg n r)) r, send_count
    | Rcv (x, _) -> (* check in_q for data *)
      begin
      match in_q with
        | [] -> (* printf "prog %d - blocked\n" prog_id ; *) Block, out_q, in_q, r, send_count
        | hd :: tl -> (* printf "(%d) received.. %d\n" prog_id hd ; *) Step 1, out_q, tl, Registers.add x hd r, send_count
      end
    | Jgz (x, n) -> if (get_val_or_reg x r) > 0 then (Step (get_val_or_reg n r), out_q, in_q, r, send_count) else (Step 1, out_q, in_q, r, send_count)


let concurrent_execution instructions =
  let rec execute prog_id idx out_q in_q r block_count send_count =
    try
      let instruction = instructions.(idx) in
      let step, out_q', in_q', r', send_count' = process_instruction prog_id instruction out_q in_q r send_count in
      match step with
        | Block -> idx, out_q', in_q', r', (block_count + 1), send_count
        | Step n -> execute prog_id (n + idx) out_q' in_q' r' 0 send_count'
    with Invalid_argument _ -> idx, out_q, in_q, r, (block_count + 1), send_count in

  let rec aux idx0 idx1 q0 q1 r0 r1 block0 block1 send_count0 send_count1 =
    let idx0, q0, q1, r0, block0, send_count0 = execute 0 idx0 q0 q1 r0 block0 send_count0 in
    let idx1, q1, q0, r1, block1, send_count1 = execute 1 idx1 q1 q0 r1 block1 send_count1 in
    if (block0 > 4) && (block1 > 4) then failwith (sprintf "DEADLOCKED %d %d" send_count0 send_count1)
    else
      (*printf "EXECUTED %d %d (%d) (%d)\n" idx0 idx1 block0 block1 ; *)
      aux idx0 idx1 q0 q1 r0 r1 block0 block1 send_count0 send_count1 in
  aux 0 0 [] [] Registers.empty Registers.empty 0 0 0 0


let () =
  let instructions = gen_instructions test_data |> Array.of_list in
  concurrent_execution instructions
