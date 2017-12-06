#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day05.native'
./day05.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day05.d.byte'

