#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day13.native'
./day13.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day13.d.byte'

