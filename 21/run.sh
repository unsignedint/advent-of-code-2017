#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day21.native'
./day21.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day21.d.byte'
#./day21.d.byte
