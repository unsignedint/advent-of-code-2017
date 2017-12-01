#!/bin/bash
set -e
ocamlbuild -pkgs extlib 'day01.native'
./day01.native

