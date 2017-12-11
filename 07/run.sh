#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day07.native'
./day07.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day07.d.byte'

