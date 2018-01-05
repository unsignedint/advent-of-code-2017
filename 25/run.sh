#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day25.native'
./day25.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day25.d.byte'
