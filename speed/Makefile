# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

# Set LEXER to: lex-utf8, wlex-utf8, ulex-utf8

REQ=str pxp-engine pxp-lex-iso88591 pxp-$(LEXER)

all: pxpvalidate.byte pxpvalidate.opt pxpevparse.byte pxpevparse.opt

pxpvalidate.opt: validate.ml
	ocamlfind ocamlopt -o pxpvalidate.opt -package "$(REQ)" -linkpkg validate.ml

pxpvalidate.byte: validate.ml
	ocamlfind ocamlc -o pxpvalidate.byte -package "$(REQ)" -linkpkg validate.ml

pxpevparse.opt: evparse.ml
	ocamlfind ocamlopt -o pxpevparse.opt -package "$(REQ)" -linkpkg evparse.ml

pxpevparse.byte: evparse.ml
	ocamlfind ocamlc -o pxpevparse.byte -package "$(REQ)" -linkpkg evparse.ml

#----------------------------------------------------------------------
.PHONY: all
all:

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa
	rm -f pxpvalidate.opt pxpvalidate.byte
	rm -f pxpevparse.opt pxpevparse.byte

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~ 

