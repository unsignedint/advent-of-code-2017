#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day06.native'
./day06.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day06.d.byte'

