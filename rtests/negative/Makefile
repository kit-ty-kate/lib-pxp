# make test_negative:   make bytecode executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

test_negative: test_negative.ml
	ocamlfind ocamlc -g -a -o test_negative.cma -package pxp,str test_negative.ml
	../create-wrapper test_negative

#----------------------------------------------------------------------
.PHONY: all
all:

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa current.out
	rm -f test_negative 

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~


