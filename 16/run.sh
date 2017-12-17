#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day16.native'
./day16.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day16.d.byte'

