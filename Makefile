
.PHONY : compile
compile:
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex scanner.mll
	ocamlc -c scanner.ml
	ocamlc -c sast.ml
	ocamlc -c semantics.ml
	ocamlc -c compiler.ml
	ocamlc -o compiler ast.cmo parser.cmo scanner.cmo sast.cmo semantics.cmo compiler.cmo
clean:
	rm -f parser.ml ast.cmi ast.cmo compiler.cmi compiler.cmo parser.cmi parser.cmo parser.mli parser.ml sast.cmi sast.cmo scanner.cmi scanner.cmo scanner.ml semantics.cmi semantics.cmo compiler	
 
