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
  Snd of string | Set of (string * int) | Add of (string * int) | Mul of (string * string) |
  Mod of (string * int) | Rcv of string | Jgz of (string * int) | JgzR of (string * string)

let gen_instructions lst =
  let process instr =
    let pieces = String.split_on_char ' ' instr in
    match pieces with
      | "snd" :: x :: [] -> Snd x
      | "set" :: x :: n :: [] -> Set (x, int_of_string n)
      | "add" :: x :: n :: [] -> Add (x, int_of_string n)
      | "mul" :: x :: y :: [] -> Mul (x, y)
      | "mod" :: x :: n :: [] -> Mod (x, int_of_string n)
      | "rcv" :: x :: [] -> Rcv x
      | "jgz" :: x :: n :: [] -> begin
        try Jgz (x, int_of_string n)
        with Failure _ -> JgzR (x, n)
      end
      | _ -> failwith "unknown?" in
  List.map process lst

let print_instruction = function
  | Snd x -> sprintf "snd(%s)" x
  | Set (x, n) -> sprintf "set(%s)=>%d" x n
  | Add (x, n) -> sprintf "add(%s)=>%d" x n
  | Mul (x, y) -> sprintf "mul(%s)=>%s" x y
  | Mod (x, n) -> sprintf "mod(%s)=>%d" x n
  | Rcv x -> sprintf "rcv(%s)" x
  | Jgz (x, n) -> sprintf "jgz(%s)=>%d" x n
  | JgzR (x, y) -> sprintf "jgz REG(%s)=>%s" x y

let process_instrunctions lst =
  let instructions = Array.of_list lst in
  let aux idx r =
    printf "idx=%d %s\n" idx (print_instruction instructions.(idx));
    match instructions.(idx) with
      | Snd x -> 1, Registers.add "snd" (reg_get_or_zero x r) r
      | Set (x, n) -> 1, Registers.add x n r
      | Add (x, n) -> let curr = reg_get_or_zero x r in 1, Registers.add x (curr + n) r
      | Mul (x, y) -> let curr = reg_get_or_zero x r in 1, Registers.add x (curr * (reg_get_or_zero y r)) r
      | Mod (x, n) -> let curr = reg_get_or_zero x r in 1, Registers.add x (curr mod n) r
      | Rcv x -> if (reg_get_or_zero x r) <> 0 then failwith (sprintf "rcv %d" (reg_get_or_zero "snd" r)) else (1, r)
      | Jgz (x, n) -> if (reg_get_or_zero x r) > 0 then (n, r) else (1, r)
      | JgzR (x, y) -> if (reg_get_or_zero x r) > 0 then ((reg_get_or_zero y r), r) else (1, r) in
  let rec go idx r =
    if idx < 0 || idx >= (List.length lst) then r
    else let step, r' = aux idx r in go (step + idx) r' in
  go 0 Registers.empty


let () =
  let instructions = gen_instructions test_data in
  let _ = process_instrunctions instructions in
  ()
