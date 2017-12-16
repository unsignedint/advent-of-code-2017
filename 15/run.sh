#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day15.native'
./day15.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day15.d.byte'

