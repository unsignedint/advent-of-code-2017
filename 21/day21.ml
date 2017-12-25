(* day 21 *)

open Printf
open ExtString

type 'a mat = { n: int; m: int; t: 'a array array }
let create_mat n m e = { n=n; m=m; t = Array.make_matrix n m e }
let access_mat m i j = m.t.(i).(j)
let mod_mat m i j e = m.t.(i).(j) <- e
let fst_mat m = m.t.(0).(0)

module FM = Map.Make(struct
  type t = char mat
  let compare = compare
end)

let print_mat m =
  for i=0 to m.n-1 do
    (* printf "%02d: " i ; *)
    for j=0 to m.m-1 do
      printf "%c" m.t.(i).(j)
    done ;
    printf "\n"
  done ; printf "\n"

let print_mat_prefix prefix m =
  print_endline prefix ; print_mat m

let slice_mat m i j width height =
  let m' = create_mat width height (fst_mat m) in
  for i' = i to i+width-1 do
    for j' = j to j+height-1 do
      m'.t.(i'-i).(j'-j) <- m.t.(i').(j')
    done
  done ; m'

let apply_mat m f =
  let m' = create_mat m.n m.m (fst_mat m) in
  for i = 0 to m.n-1 do
    for j = 0 to m.m-1 do
      f m' i j
    done
  done ; m'

let equal_mat p q =
  Array.for_all (fun p -> p) (Array.map2 (fun a b -> Array.for_all (fun p -> p) (Array.map2 (fun x y -> x = y) a b)) p.t q.t)

let flip_hor_mat m = apply_mat m (fun m' i j -> m'.t.(m.n-1-i).(j) <- m.t.(i).(j))
let flip_vert_mat m = apply_mat m (fun m' i j -> m'.t.(i).(m.m-1-j) <- m.t.(i).(j))
let rotate_mat m = apply_mat m (fun m' i j -> m'.t.(j).(i) <- m.t.(i).(j)) |> flip_hor_mat

let concat_vert_mat p q =
  (* print_mat_prefix "concat_vert_mat p" p ; *)
  (* print_mat_prefix "concat_vert_mat p" p ; *)
  let m' = create_mat (p.n+q.n) (p.m) (fst_mat p) in
  for i = 0 to p.n-1 do
    for j = 0 to p.m-1 do
      m'.t.(i).(j) <- p.t.(i).(j) ;
    done
  done ;
  for i = 0 to q.n-1 do
    for j = 0 to q.m-1 do
      m'.t.(i+p.n).(j) <- q.t.(i).(j)
    done
  done ; m'

let concat_hor_mat p q =
  (* print_mat_prefix "concat_hor_mat p" p ; *)
  (* print_mat_prefix "concat_hor_mat q" q ; *)
  let m' = create_mat (p.n) (p.m+q.m) (fst_mat p) in
  for i = 0 to p.n-1 do
    for j = 0 to p.m-1 do
      m'.t.(i).(j) <- p.t.(i).(j) ;
    done
  done ;
  for i = 0 to q.n-1 do
    for j = 0 to q.m-1 do
      m'.t.(i).(j+p.m) <- q.t.(i).(j)
    done
  done ; m'

let lines_to_mat slist =
  let t = Array.of_list (List.map (fun x -> String.explode x |> Array.of_list) slist) in
  let n = Array.length t.(0) in
  let m = Array.length t in
  {n; m; t}

let delim_line_to_mat ?delim:(delim='/') s =
  let pieces = String.split_on_char delim s in
  lines_to_mat pieces

(* array of char arrays *)
let test_data = Std.input_list (open_in "input.txt")

let process_data lines =
  let process_line line m =
    let pieces = String.split_on_char ' ' (String.strip line) in
    let k, v = delim_line_to_mat (List.hd pieces), delim_line_to_mat (List.nth pieces 2) in
    FM.add k v m in
  List.fold_right process_line lines FM.empty


let split_mat m amount =
  let rec split_hor x y acc =
    if x < m.m then
      let m' = slice_mat m x y amount amount in
      split_hor (x + amount) y (m' :: acc)
    else
      acc in
  let rec split_vert y acc =
    if y < m.n then
      let row = split_hor 0 y [] in
      split_vert (y + amount) ((List.rev row) :: acc)
    else
      List.rev acc in
  split_vert 0 []

let unflatten lst n =
  (* printf "Length of lst = %d (n = %d)\n" (List.length lst) n ; *)
  (* let amount = (List.length lst) / n in *)
  let rec aux acc' acc i row = function
    | [] ->
      begin
      (* List.iteri (fun i x -> (printf "acc %d/%d..\n" row i ; print_mat x)) acc ; *)
      (List.rev acc) :: acc'
      end
    | (x :: xs) as lst' ->
      if i < n then
        aux acc' (x :: acc) (i + 1) row xs
      else
        begin
        (* List.iteri (fun i x -> (printf "acc %d/%d..\n" row i ; print_mat x)) acc ; *)
        aux ((List.rev acc) :: acc') [] 0 (row+1) lst'
        end
        in
  List.rev (aux [] [] 0 0 lst)

let combine_lists_to_matrix lst =
  (* printf "combine_lists_to_matrix len = %d\n" (List.length lst) ; *)
  let rows = List.map (fun c -> List.fold_left concat_vert_mat (List.hd c) (List.tl c)) lst in
  List.fold_left concat_hor_mat (List.hd rows) (List.tl rows)


let better_repeat f a n =
  let rec aux acc n =
    if n = 0 then acc
    else aux (f acc) (n-1)
  in aux a n

let mat_enhancement t m =
  (* start from 0 to basically add the original at the front *)
  let c1 = List.map (better_repeat rotate_mat m) [0; 1; 2; 3] in
  let c2 = List.map (better_repeat rotate_mat (flip_hor_mat m)) [0; 1; 2; 3] in
  let combinations = c1 @ c2 in
  (* List.iter (print_mat_prefix "combinations") combinations ; *)
  let hit = List.filter (fun x -> FM.mem x t) combinations in
  match hit with
    | [] -> (print_mat m ; failwith "nothing!?")
    | x :: _ -> FM.find x t
  (* return replacement *)
  (* FM.find (List.hd hit) t *)


let make_fractal t fractal =
  let factor =
    if fractal.n mod 2 = 0 then 2
    else if fractal.n mod 3 = 0 then 3
    else failwith "indivisible by 2/3!" in
  let splits = split_mat fractal factor in
  let num_splits = List.length splits in
  let matrix_pieces = List.flatten splits in
  let new_pieces = List.map (mat_enhancement t) matrix_pieces in
  (* List.iteri (fun i x -> (printf "new_pieces %d..\n" i ; print_mat x)) new_pieces ; *)
  combine_lists_to_matrix (unflatten new_pieces num_splits)


let fold_left_on_mat f acc m =
  let acc' = ref acc in
  for i = 0 to m.n-1 do
    for j = 0 to m.m-1 do
      acc' := f !acc' m.t.(i).(j)
    done
  done ; !acc'

let repeat_and_count t m =
  let r = better_repeat (make_fractal t) m 3 in
  fold_left_on_mat (fun a b -> if b = '#' then a + 1 else a) 0 r

let () =
  let t = process_data test_data in
  let start_fractal = delim_line_to_mat ".#./..#/###" in
  let first_result = better_repeat (make_fractal t) start_fractal 5 in
  print_mat_prefix "five result" first_result ;
  let num_bits = fold_left_on_mat (fun a b -> if b = '#' then a + 1 else a) 0 first_result in
  printf "num bits = %d\n" num_bits ;
  let final_result = better_repeat (make_fractal t) first_result 10 in
  let num_bits = fold_left_on_mat (fun a b -> if b = '#' then a + 1 else a) 0 final_result in
  printf "num bits = %d\n" num_bits ;

  (* we know that after 15 iterations the width is 486, so we split into 4 smaller pieces *)
  let big_pieces = List.flatten (split_mat final_result 243) in
  let total = List.fold_left (+) 0 (List.map (repeat_and_count t) big_pieces) in
  printf "num bits (total) = %d\n" total ;




(*
  let start_fractal = delim_line_to_mat "#..#/..../#..#/.##." in
  let splits = split_mat start_fractal 2 in
  (* List.iter print_mat (List.flatten splits) ; *)
  let unflattened = unflatten (List.flatten splits) 2 in
  List.iter (fun p -> (printf "next\n" ; List.iter print_mat p)) unflattened ;
  print_mat (combine_lists_to_matrix unflattened) *)

  (*
let () =
  let test_mat = lines_to_mat test_data in
  let a = rotate_mat test_mat |> rotate_mat |> rotate_mat in
  print_mat test_mat ;
  print_mat a ;
  printf "equal = %s\n" (if equal_mat test_mat a then "yes" else "no") ;
  (* let a = slice_mat test_mat 2 2 2 2 in *)
  (* print_mat a ; *)
  (* print_mat (rotate_mat test_mat) ;
  print_mat (rotate_mat test_mat |> rotate_mat) ;
  print_mat (rotate_mat test_mat |> rotate_mat |> rotate_mat) ;
  let test_mat = flip_vert_mat test_mat in
  print_mat test_mat ;
  print_mat (rotate_mat test_mat) ;
  print_mat (rotate_mat test_mat |> rotate_mat) ;
  print_mat (rotate_mat test_mat |> rotate_mat |> rotate_mat) ; *)
  print_mat (concat_hor_mat test_mat test_mat) ;
  print_mat (concat_vert_mat (concat_hor_mat test_mat test_mat) (concat_hor_mat test_mat test_mat)) ;
  let foo = "#..#/..../#..#/.##." in
  let new_mat = delim_line_to_mat foo in
  print_mat new_mat ;
*)


(* let () =
  let test_mat = delim_line_to_mat ".#./..#/###" in
  print_mat test_mat ;
  print_endline "combinations.." ;
  let combinations = List.map (repeat rotate_mat test_mat) [0; 1; 2; 3] in
  List.iter print_mat combinations ; *)
