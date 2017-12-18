#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day18.native'
./day18.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day18.d.byte'

