# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

OCAMLPATH=../..

.PHONY: all
all: test_write test_namespace

test_write: test_write.ml
	ocamlfind ocamlc -g -custom -o test_write -package .,str -linkpkg test_write.ml

test_namespace: test_namespace.ml
	ocamlfind ocamlc -g -custom -o test_namespace -package .,str -linkpkg test_namespace.ml -predicates pxp_without_utf8

#----------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa out1 out2 out3

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f test_write test_namespace

