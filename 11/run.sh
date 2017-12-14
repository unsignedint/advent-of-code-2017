#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day11.native'
./day11.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day11.d.byte'

