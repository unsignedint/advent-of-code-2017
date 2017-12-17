#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day17.native'
./day17.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day17.d.byte'

