#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day10.native'
./day10.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day10.d.byte'

