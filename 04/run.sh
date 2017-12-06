#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day04.native'
./day04.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day04.d.byte'

