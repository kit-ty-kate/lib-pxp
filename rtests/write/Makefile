# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

.PHONY: all
all: test_write test_namespace

test_write: test_write.cma
	../create-wrapper test_write

test_namespace: test_namespace.cma
	../create-wrapper test_namespace

test_write.cma: test_write.ml
	ocamlfind ocamlc -a -g -o test_write.cma -package pxp,str test_write.ml

test_namespace.cma: test_namespace.ml
	ocamlfind ocamlc -a -g -o test_namespace.cma -package pxp,str test_namespace.ml

#----------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa out1 out2 out3
	rm -f test_write test_namespace

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~

