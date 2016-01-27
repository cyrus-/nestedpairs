#!/bin/sh

ocamlbuild -use-ocamlfind \
  -pkg js_of_ocaml \
  -pkg tyxml \
  -pkg js_of_ocaml.tyxml \
  index.byte ;

js_of_ocaml +weak.js -o index.js index.byte
