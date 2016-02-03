#!/bin/sh

ocamlbuild -use-ocamlfind \
  -pkg js_of_ocaml \
  -pkg tyxml \
  -pkg js_of_ocaml.tyxml \
  -pkg js_of_ocaml.syntax \
  -syntax camlp4o \
  index.byte ;

js_of_ocaml +weak.js -o index.ml.js index.byte
