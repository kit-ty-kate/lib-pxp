# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

.PHONY: all
all: test_reader test_rewritten_reader

test_reader: test_reader.ml minilex.ml
	ocamlfind ocamlc -o test_reader -package pxp,unix,threads \
		-linkpkg -thread \
		-g minilex.ml test_reader.ml

test_rewritten_reader: test_rewritten_reader.ml minilex.ml
	ocamlfind ocamlc -o test_rewritten_reader -package pxp,unix \
		-linkpkg \
		-g minilex.ml test_rewritten_reader.ml

minilex.ml: minilex.mll
	ocamllex minilex.mll

#----------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa minilex.ml

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f test_reader test_rewritten_reader

