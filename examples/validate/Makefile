# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

# REQ: This variable is set in Makefile.config

pxpvalidate: validate.ml
	ocamlfind ocamlc -g -o pxpvalidate -package "$(REQ)" -linkpkg validate.ml

pxpvalidate.opt: validate.ml
	ocamlfind ocamlopt -o pxpvalidate.opt -package "$(REQ)" -linkpkg validate.ml

# The following rules to create the bytecode executable assume O'Caml 3.03:

pxpvalidate.byte: validate.ml
	ocamlfind ocamlc -o pxpvalidate.byte -package "$(REQ)" -linkpkg validate.ml

pxpvalidate.1: pxpvalidate.pod
	pod2man --section=1 --center "www.ocaml-programming.de" --release 'PXP 1.1 tools' --lax pxpvalidate.pod >pxpvalidate.1

releasebyte:
	(cd ..; tar czf pxpvalidate-1.1.1.tar.gz pxpvalidate/{INSTALL,LICENSE,Makefile,pxpvalidate.1,pxpvalidate.byte,setup.sh,validate.ml})


Makefile.config:
	if ocamlfind query pxp-wlex-utf8 >/dev/null; then       \
	    lexer="pxp-lex-iso88591 pxp-wlex-utf8";             \
	else                                                    \
	    lexer="pxp-lex-iso88591 pxp-lex-utf8";              \
	fi;                                                     \
	echo "REQ = str pxp-engine $$lexer" >Makefile.config

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
	rm -f *~ Makefile.config
	rm -f pxpvalidate pxpvalidate.opt pxpvalidate.byte interpreter

include Makefile.config
