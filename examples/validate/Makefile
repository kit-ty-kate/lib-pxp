# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

pxpvalidate: validate.ml
	ocamlfind ocamlc -g -o pxpvalidate -package "pxp,unix,str" -linkpkg validate.ml

pxpvalidate.opt: validate.ml
	ocamlfind ocamlopt -o pxpvalidate.opt -package "pxp,unix,str" -linkpkg validate.ml

BYTEPKG = pxp-engine pxp-lex-iso88591 pxp-lex-utf8

# The following rules to create the bytecode executable assume O'Caml 3.03:

pxpvalidate.byte: validate.ml
	ocamlfind ocamlc -o pxpvalidate.byte -package "pxp,unix,str" -linkpkg validate.ml

pxpvalidate.1: pxpvalidate.pod
	pod2man --section=1 --center "www.ocaml-programming.de" --release 'PXP 1.1 tools' --lax pxpvalidate.pod >pxpvalidate.1

releasebyte:
	(cd ..; tar czf pxpvalidate-1.1.1.tar.gz pxpvalidate/{INSTALL,LICENSE,Makefile,pxpvalidate.1,pxpvalidate.byte,setup.sh,validate.ml})

#----------------------------------------------------------------------
.PHONY: all
all:

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa 

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f pxpvalidate pxpvalidate.opt pxpvalidate.byte interpreter
