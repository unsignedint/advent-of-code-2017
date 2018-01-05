(* day 25 *)

open Printf
open ExtString

type move = Left of int | Right of int
type state = {
  label: char;
  curr: int;
  write: int;
  move: move;
  next: char;
}

let state_table = [
  {label='A'; curr=0; write=1; move=Right(1); next='B'};
  {label='A'; curr=1; write=1; move=Left(1); next='E'};
  {label='B'; curr=0; write=1; move=Right(1); next='C'};
  {label='B'; curr=1; write=1; move=Right(1); next='F'};
  {label='C'; curr=0; write=1; move=Left(1); next='D'};
  {label='C'; curr=1; write=0; move=Right(1); next='B'};
  {label='D'; curr=0; write=1; move=Right(1); next='E'};
  {label='D'; curr=1; write=0; move=Left(1); next='C'};
  {label='E'; curr=0; write=1; move=Left(1); next='A'};
  {label='E'; curr=1; write=0; move=Right(1); next='D'};
  {label='F'; curr=0; write=1; move=Right(1); next='A'};
  {label='F'; curr=1; write=1; move=Right(1); next='C'};
  ]
(* let example_state_table = [
  {label='A'; curr=0; write=1; move=Right(1); next='B'};
  {label='A'; curr=1; write=0; move=Left(1); next='B'};
  {label='B'; curr=0; write=1; move=Left(1); next='A'};
  {label='B'; curr=1; write=1; move=Right(1); next='A'};
] *)

module Tape = Map.Make(struct
  type t = int
  let compare = compare
end)

let execute acc =
  let tape, cursor, state = acc in
  let current_value = match Tape.find_opt cursor tape with | Some v -> v | None -> 0 in
  let state_entry = List.find (fun {label=l; curr=c; _} -> l = state && c = current_value) state_table in
  let tape' = Tape.add cursor state_entry.write tape in
  let cursor' = match state_entry.move with
    | Right x -> cursor + x
    | Left x -> cursor - x in
  let state' = state_entry.next in
  tape', cursor', state'

let better_repeat f a n =
  let rec aux acc n =
    if n = 0 then acc
    else aux (f acc) (n-1)
  in aux a n

let () =
  let tape, cursor, state = better_repeat execute (Tape.empty, 0, 'A') 12459852 in
  printf "cursor = %d, state = %c\n" cursor state ;
  printf "checksum = %d\n" (Tape.fold (fun _ v a -> a + v) tape 0)

