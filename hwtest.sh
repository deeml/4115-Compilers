#!/bin/bash

echo "======ast.ml=========="
ocamlc ast.ml
echo "======scanner.ml======"
ocamllex scanner.mll
echo "======parser.mly======"
ocamlyacc parser.mly
echo "======parser.mli======"
ocamlc parser.mli
echo "======generator.ml===="
ocamlc generator.ml
echo "======probl.ml========"
ocamlc -o probl parser.ml scanner.ml generator.ml probl.m

#probl hello_world.probl
