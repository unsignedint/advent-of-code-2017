#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day23.native'
./day23.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day23.d.byte'
