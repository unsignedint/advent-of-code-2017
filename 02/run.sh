#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day02.native'
./day02.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day02.d.byte'

