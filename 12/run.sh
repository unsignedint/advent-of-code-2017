#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day12.native'
./day12.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day12.d.byte'

