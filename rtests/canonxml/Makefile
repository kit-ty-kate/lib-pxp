# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

test_canonxml: test_canonxml.ml
	ocamlfind ocamlc -g -custom -o test_canonxml -package pxp,str -linkpkg test_canonxml.ml

#----------------------------------------------------------------------
.PHONY: all
all:

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa out.xml

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f test_canonxml
