# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

pxpvalidate: validate.ml
	ocamlfind ocamlc -g -o pxpvalidate -package "pxp,unix" -linkpkg validate.ml

pxpvalidate.opt: validate.ml
	ocamlfind ocamlopt -o pxpvalidate.opt -package "pxp,unix" -linkpkg validate.ml

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
	rm -f pxpvalidate pxpvalidate.opt
