(* day 08 *)

open Printf
open ExtString

let file = "input2.txt"


let re_full = Str.regexp "\\([a-z]+\\) \\(inc\|dec\\) \\([0-9-]+\\) if \\([a-z]+\\) \\(>\|>=\|<\|<=\|==\|!=\\) \\([0-9-]+\\)" ;;

module Registers = Map.Make(String)

let reg_get_or_zero key m =
  match Registers.find_opt key m with
    | Some v -> v
    | None -> 0

let reg_find_max m =
  Registers.fold (fun _ v acc -> if v > acc then v else acc) m 0

let print_register k v =
  printf "%s=%d\n" k v

type operation = Inc of int | Dec of int
type comparator = Gt | GtEq | Lt | LtEq | Eq | NEq
type comparison = string * comparator * int
type instruction = string * operation * comparison  (* variable, amount *)

let print_instruction instr =
  let var, op, cmp = instr in
  let op_str = match op with
    | Inc x -> sprintf "inc(%d)" x
    | Dec x -> sprintf "dec(%d)" x in
  let cmp_str = match cmp with
  | a, Gt, b    -> sprintf "%s>%d"  a b
  | a, GtEq, b  -> sprintf "%s>=%d" a b
  | a, Lt, b    -> sprintf "%s<%d"  a b
  | a, LtEq, b  -> sprintf "%s<=%d" a b
  | a, Eq, b    -> sprintf "%s==%d" a b
  | a, NEq, b   -> sprintf "%s!=%d" a b in
  printf "var=%s op=%s cmp=%s\n" var op_str cmp_str

let process_line line =
  if Str.string_match re_full line 0 then
    let var = Str.matched_group 1 line in
    let amount = int_of_string (Str.matched_group 3 line) in
    let op = if (Str.matched_group 2 line) = "inc" then Inc (amount) else Dec (amount) in
    let cmp_var = Str.matched_group 4 line in
    let cmp_val = int_of_string (Str.matched_group 6 line) in
    let cmp_op = match (Str.matched_group 5 line) with
      | ">"   -> Gt
      | ">="  -> GtEq
      | "<"   -> Lt
      | "<="  -> LtEq
      | "=="  -> Eq
      | "!="  -> NEq
      | _ -> failwith "what?"
    in
    var, op, (cmp_var, cmp_op, cmp_val)
  else
    failwith ("can;t parse!: " ^ line)


let process_instructions instructions =
  let rec aux r instr =
    let var, op, (cmp_var, cmp_op, cmp_val) = instr in
    let cmp_var_val = reg_get_or_zero cmp_var r in
    let var_val = reg_get_or_zero var r in
    let do_update = match cmp_op with
      | Gt   when cmp_var_val >  cmp_val -> true
      | GtEq when cmp_var_val >= cmp_val -> true
      | Lt   when cmp_var_val <  cmp_val -> true
      | LtEq when cmp_var_val <= cmp_val -> true
      | Eq   when cmp_var_val == cmp_val -> true
      | NEq  when cmp_var_val <> cmp_val -> true
      | _ -> false in
    if do_update then
      match op with
        | Inc x -> Registers.add var (var_val + x) r
        | Dec x -> Registers.add var (var_val - x) r
    else
      r
  in
  List.fold_left aux Registers.empty instructions


let () =
  let raw_lines = List.map String.strip (Std.input_list (open_in file)) in
  let instructions = List.map process_line raw_lines in
  (List.iter print_instruction instructions ;
  let registers = process_instructions instructions in
  Registers.iter print_register registers ;
  printf "max %d\n" (reg_find_max registers)
  )
