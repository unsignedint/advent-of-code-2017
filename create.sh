#!/bin/bash
DAY_NUM=$(printf "%02d" $1)
DAY="day$DAY_NUM"
mkdir $DAY_NUM
cat <<EOF > $DAY_NUM/$DAY.ml
(* day $DAY_NUM *)

open Printf
open ExtString

let test_data = List.map String.strip (Std.input_list (open_in "input.txt"))

let () =

EOF
cat <<EOF > $DAY_NUM/run.sh
#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str '$DAY.native'
./$DAY.native
# do this for debug build
#ocamlbuild -pkgs extlib,str '$DAY.d.byte'
EOF
chmod +x $DAY_NUM/run.sh
