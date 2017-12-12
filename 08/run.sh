#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day08.native'
./day08.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day08.d.byte'

