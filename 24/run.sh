#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day24.native'
./day24.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day24.d.byte'
