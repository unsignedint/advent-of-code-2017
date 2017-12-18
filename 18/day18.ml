(* day 17 *)

open Printf
open ExtString

let test_data = List.map String.strip (Std.input_list (open_in "input.txt"))

module Registers = Map.Make(String)

let reg_get_or_zero key m =
  match Registers.find_opt key m with
    | Some v -> v
    | None -> 0

type instruction =
  Snd of (string * string) | Set of (string * string) | Add of (string * string) | Mul of (string * string) |
  Mod of (string * string) | Rcv of (string * string) | Jgz of (string * string)

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

let print_instruction = function
  | Snd (x, _) -> sprintf "snd(%s)" x
  | Set (x, n) -> sprintf "set(%s)=>%s" x n
  | Add (x, n) -> sprintf "add(%s)=>%s" x n
  | Mul (x, y) -> sprintf "mul(%s)=>%s" x y
  | Mod (x, n) -> sprintf "mod(%s)=>%s" x n
  | Rcv (x, _) -> sprintf "rcv(%s)" x
  | Jgz (x, n) -> sprintf "jgz(%s)=>%s" x n

let get_val_or_reg unk r =
  try int_of_string unk
  with Failure _ -> reg_get_or_zero unk r

let process_instrunctions lst =
  let instructions = Array.of_list lst in
  let aux idx r =
    printf "idx=%d %s\n" idx (print_instruction instructions.(idx));
    match instructions.(idx) with
      | Snd (x, _) -> 1, Registers.add "snd" (get_val_or_reg x r) r
      | Set (x, n) -> 1, Registers.add x (get_val_or_reg n r) r
      | Add (x, n) -> let curr = reg_get_or_zero x r in 1, Registers.add x (curr + (get_val_or_reg n r)) r
      | Mul (x, y) -> let curr = reg_get_or_zero x r in 1, Registers.add x (curr * (get_val_or_reg y r)) r
      | Mod (x, n) -> let curr = reg_get_or_zero x r in 1, Registers.add x (curr mod (get_val_or_reg n r)) r
      | Rcv (x, _) -> if (reg_get_or_zero x r) <> 0 then failwith (sprintf "rcv %d" (reg_get_or_zero "snd" r)) else (1, r)
      | Jgz (x, n) -> if (reg_get_or_zero x r) > 0 then ((get_val_or_reg n r), r) else (1, r) in
  let rec go idx r =
    if idx < 0 || idx >= (List.length lst) then r
    else let step, r' = aux idx r in go (step + idx) r' in
  go 0 Registers.empty


let () =
  let instructions = gen_instructions test_data in
  let _ = process_instrunctions instructions in
  ()
