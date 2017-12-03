#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day03.native'
./day03.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day03.d.byte'

