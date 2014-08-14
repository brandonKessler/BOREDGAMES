#!/bin/sh
ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c compiler.ml
ocamlc -o compile ast.cmo parser.cmo scanner.cmo compiler.cmo

