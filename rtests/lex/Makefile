all: test_lex_utf8

# OCAMLPATH=../../gensrc:../../src


test_lex_utf8: test_lex_utf8.ml
	ocamlfind ocamlc -o test_lex_utf8 -package pxp-lex-utf8 -linkpkg \
		test_lex_utf8.ml

clean:
	rm -rf test_lex_utf8 *.cmo *.cmi

CLEAN: clean

distclean: clean
