all: test_lex_utf8

# OCAMLPATH=../../gensrc:../../src


test_lex_utf8: test_lex_utf8.ml
	ocamlfind ocamlc -o test_lex_utf8.cma -a -package pxp-lex-utf8 \
		test_lex_utf8.ml
	../create-wrapper test_lex_utf8

clean:
	rm -rf test_lex_utf8 *.cmo *.cmi

CLEAN: clean

distclean: clean
