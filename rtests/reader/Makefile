# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

.PHONY: all
all: test_reader test_rewritten_reader

test_reader: test_reader.cma
	../create-wrapper test_reader

test_rewritten_reader: test_rewritten_reader.cma
	../create-wrapper test_rewritten_reader

test_reader.cma: test_reader.ml minilex.ml
	ocamlfind ocamlc -a -o test_reader.cma -package pxp,unix,threads \
		-thread \
		-g minilex.ml test_reader.ml

test_rewritten_reader.cma: test_rewritten_reader.ml minilex.ml
	ocamlfind ocamlc -a -o test_rewritten_reader.cma -package pxp,unix \
		-g minilex.ml test_rewritten_reader.ml

minilex.ml: minilex.mll
	ocamllex minilex.mll

#----------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa minilex.ml
	rm -f test_reader test_rewritten_reader

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~


