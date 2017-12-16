(* day 15 *)

open Printf
open ExtString

let seed_a = 512
let seed_b = 191

let generator factor x = (x * factor) mod 2147483647

let rec duel acc gen_a gen_b a b limit =
  let a' = gen_a a in
  let b' = gen_b b in
  let acc' = if (a' land 65535) = (b' land 65535) then acc + 1 else acc in
  if limit = 1 then acc'
  else duel acc' gen_a gen_b a' b' (limit - 1)

let duel_part_one limit =
  let a_generator = generator 16807 in
  let b_generator = generator 48271 in
  duel 0 a_generator b_generator seed_a seed_b (limit - 1)

let rec generator2 factor multiple x =
  let x' = (x * factor) mod 2147483647 in
  if x' mod multiple = 0 then x' else generator2 factor multiple x'

let duel_part_two limit =
  let a_generator = generator2 16807 4 in
  let b_generator = generator2 48271 8 in
  duel 0 a_generator b_generator seed_a seed_b (limit - 1)

let () =
  printf "part1 = %d\n" (duel_part_one 40_000_000) ;
  printf "part2 = %d\n" (duel_part_two 5_000_000) ;
