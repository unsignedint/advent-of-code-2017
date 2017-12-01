(* day 01, part 1 *)

open Printf
open ExtString

let file = "input.dat"

let do_the_thing aa =
    let exploded_input = String.explode aa in
    let digits = List.map (fun x -> Char.code x - 0x30) exploded_input in
    let first_digit = List.hd digits in
    let rec aux acc = function
    | [] -> acc
    | [b] -> if b = first_digit then acc + b else acc
    | a :: (b :: _ as xs) ->
        if a = b then aux (acc + a) xs else aux acc xs
    in
    aux 0 digits

let () =
    let raw_input = String.strip (Std.input_file file) in
    print_endline (string_of_int (do_the_thing raw_input))
