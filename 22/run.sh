#!/bin/bash
set -e
# running part 2
ocamlbuild -pkgs extlib,str 'day22b.native'
./day22b.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day22.d.byte'
