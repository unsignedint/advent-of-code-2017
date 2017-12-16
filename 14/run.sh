#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day14.native'
./day14.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day14.d.byte'

