#!/bin/bash
set -e
ocamlbuild -pkgs extlib,str 'day19.native'
./day19.native
# do this for debug build
#ocamlbuild -pkgs extlib,str 'day19.d.byte'
