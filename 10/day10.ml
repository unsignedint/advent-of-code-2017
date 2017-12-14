(* day 10 *)

open Printf
open ExtString

(* let input_data = [0;1;2;3;4] *)
(* let input_lenths = [3;4;1;5] *)

let input_data = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32;33;34;35;36;37;38;39;40;41;42;43;44;45;46;47;48;49;50;51;52;53;54;55;56;57;58;59;60;61;62;63;64;65;66;67;68;69;70;71;72;73;74;75;76;77;78;79;80;81;82;83;84;85;86;87;88;89;90;91;92;93;94;95;96;97;98;99;100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;115;116;117;118;119;120;121;122;123;124;125;126;127;128;129;130;131;132;133;134;135;136;137;138;139;140;141;142;143;144;145;146;147;148;149;150;151;152;153;154;155;156;157;158;159;160;161;162;163;164;165;166;167;168;169;170;171;172;173;174;175;176;177;178;179;180;181;182;183;184;185;186;187;188;189;190;191;192;193;194;195;196;197;198;199;200;201;202;203;204;205;206;207;208;209;210;211;212;213;214;215;216;217;218;219;220;221;222;223;224;225;226;227;228;229;230;231;232;233;234;235;236;237;238;239;240;241;242;243;244;245;246;247;248;249;250;251;252;253;254;255]
let input_lenths = [83;0;193;1;254;237;187;40;88;27;2;255;149;29;42;100]
(* let input_str = "1,2,3" *)
let input_str = "83,0,193,1,254,237,187,40,88,27,2,255,149,29,42,100"

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
;;

let split_list lst len =
  let rec aux acc i = function
    | [] -> acc, []
    | (x :: xs) as y ->
      if i < len then aux (x :: acc) (i + 1) xs
      else List.rev acc, y in
  if List.length lst = len then lst, []
  else aux [] 0 lst
;;

let step acc len =
  let lst, curr, skip = acc in
  let a, b = split_list (rotate_list lst curr) len in
  let next_lst = rotate_list (List.rev a @ b) (List.length lst - curr) in
  let next_curr = (curr + len + skip) mod (List.length lst) in
  let next_skip = skip + 1 in
  (* printf "%s curr=%d skip=%d\n" (String.join "," (List.map string_of_int next_lst)) next_curr next_skip; *)
  next_lst, next_curr, next_skip


let process start inputs nrounds =
  let rec aux lst curr skip i =
    let a, b, c = List.fold_left step (lst, curr, skip) inputs in
    if i < nrounds then aux a b c (i + 1)
    else a, b, c in
  aux start 0 0 1


let convert_int_list s =
  let exploded = String.explode s in
  let ords = List.map Char.code exploded in
  ords @ [17;31;73;47;23]

let rec make_hash l acc i =
  let piece, tail = split_list l 16 in
  let hash = List.fold_left (lxor) 0 piece in
  if i < 16 then
    make_hash tail (hash :: acc) (i+1)
  else
    hash :: acc


let () =
  let final, _, _ = process input_data input_lenths 1 in
  printf "=> %s\n" (String.join "," (List.map string_of_int final)) ;
  printf "answer = %d\n" ((List.hd final) * (List.nth final 1)) ;
  let two, _, _ = process input_data (convert_int_list input_str) 64 in
  printf "=> %s\n" (String.join "," (List.map string_of_int two)) ;
  let hash = List.rev (make_hash two [] 1) in
  printf "=> %s\n" (String.join "," (List.map string_of_int hash)) ;
  printf "answer = %s\n" (String.join "" (List.map (fun x -> sprintf "%02x" x) hash)) ;
