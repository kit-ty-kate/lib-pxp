# make preprocess:	make bytecode executable
# make preprocess.opt: 	make native executable
# make clean: 		remove intermediate files
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files
# make install
#----------------------------------------------------------------------

BIN = /usr/local/bin

preprocess: preprocess.ml
	ocamlfind ocamlc -g -o preprocess -package pxp,str -linkpkg preprocess.ml

preprocess.opt: preprocess.ml
	ocamlfind ocamlopt -o preprocess.opt -package pxp,str -linkpkg preprocess.ml


.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~ depend depend.pkg
	rm -f preprocess preprocess.opt

.PHONY: install
install:
	cp preprocess $(BIN)
