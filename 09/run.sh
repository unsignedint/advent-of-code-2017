#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day09.native'
./day09.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day09.d.byte'

