(* day 14 *)

open Printf
open ExtString

let input_str = "wenycdww"
let input_data = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119;120;121;122;123;124;125;126;127;128;129;130;131;132;133;134;135;136;137;138;139;140;141;142;143;144;145;146;147;148;149;150;151;152;153;154;155;156;157;158;159;160;161;162;163;164;165;166;167;168;169;170;171;172;173;174;175;176;177;178;179;180;181;182;183;184;185;186;187;188;189;190;191;192;193;194;195;196;197;198;199;200;201;202;203;204;205;206;207;208;209;210;211;212;213;214;215;216;217;218;219;220;221;222;223;224;225;226;227;228;229;230;231;232;233;234;235;236;237;238;239;240;241;242;243;244;245;246;247;248;249;250;251;252;253;254;255]

(* START - DAY 10, KNOT HASH *)
let rotate_list lst n =
  let adjusted_n = n mod (List.length lst) in
  let rec aux acc i = function
    | [] -> acc
    | x :: xs as t ->
      if i > adjusted_n then
        t @ acc
      else
        aux (acc @ x :: []) (i + 1) xs
  in aux [] 1 lst

let split_list lst len =
  let rec aux acc i = function
    | [] -> acc, []
    | (x :: xs) as y ->
      if i < len then aux (x :: acc) (i + 1) xs
      else List.rev acc, y in
  if List.length lst = len then lst, []
  else aux [] 0 lst

let step acc len =
  let lst, curr, skip = acc in
  let a, b = split_list (rotate_list lst curr) len in
  let next_lst = rotate_list (List.rev a @ b) (List.length lst - curr) in
  let next_curr = (curr + len + skip) mod (List.length lst) in
  let next_skip = skip + 1 in
  (* printf "%s curr=%d skip=%d\n" (String.join "," (List.map string_of_int next_lst)) next_curr next_skip; *)
  next_lst, next_curr, next_skip

let process start nrounds inputs =
  let rec aux lst curr skip i =
    let a, b, c = List.fold_left step (lst, curr, skip) inputs in
    if i < nrounds then aux a b c (i + 1)
    else a, b, c in
  let lst, _, _ = aux start 0 0 1 in
  lst

let convert_int_list s =
  let exploded = String.explode s in
  let ords = List.map Char.code exploded in
  ords @ [17;31;73;47;23]

let make_hash input =
  let rec aux l acc i =
    let piece, tail = split_list l 16 in
    let hash = List.fold_left (lxor) 0 piece in
    if i < 16 then
      aux tail (hash :: acc) (i+1)
    else
      hash :: acc in
  List.rev (aux input [] 1)

(* END - DAY 10, KNOT HASH *)

let count_bits x =
  let rec aux acc a =
    if a = 0 then acc
    else if a land 1 = 1 then aux (acc + 1) (a lsr 1)
    else aux acc (a lsr 1) in
  aux 0 x

let int_to_bit_array x =
  let acc = Array.make 8 0 in
  let rec aux a pos =
    if a = 0 then acc
    else begin
      Array.set acc pos (a land 1) ;
      aux (a lsr 1) (pos - 1) end in
  aux x 7

let make_grid dense_hashes =
  Array.of_list (List.map (fun row -> Array.concat (List.map int_to_bit_array row)) dense_hashes),
  Array.make_matrix 128 128 false

let make_range n =
  let rec aux acc i =
    if i = -1 then acc
    else aux (i :: acc) (i - 1) in
  aux [] (n - 1)

let find_regions grid seen =
  let rec build_region pos =
    let x, y = pos in
    if x >= 0 && x < 128 && y >= 0 && y < 128 && grid.(x).(y) = 1 && not seen.(x).(y) then begin
      seen.(x).(y) <- true ;
      List.iter build_region [x+1,y; x-1,y; x,y+1; x,y-1]
    end in
  let rec aux acc x y =
    let acc' = if grid.(x).(y) = 1 && not seen.(x).(y) then (build_region (x, y) ; acc + 1) else acc in
    if x = 127 && y = 127 then acc'
    else if x = 127 then aux acc' 0 (y+1)
    else aux acc' (x+1) y in
  aux 0 0 0

let () =
  let input_strings = List.map (fun x -> input_str ^ (sprintf "-%d" x)) (make_range 128) in
  let inputs = List.map convert_int_list input_strings in
  let sparse_hashes = List.map (process input_data 64) inputs in
  let dense_hashes = List.map make_hash sparse_hashes in
  let counted_bits = List.map (List.fold_left (fun acc x -> acc + (count_bits x)) 0) dense_hashes in
  let num_bits = List.fold_left (+) 0 counted_bits in
  printf "num_bits = %d\n" num_bits ;
  let grid, seen = make_grid dense_hashes in
  let num_regions = find_regions grid seen in
  printf "num_regions = %d\n" num_regions ;
