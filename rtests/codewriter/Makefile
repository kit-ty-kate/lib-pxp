# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

OCAMLPATH=../../src

all: compile marshal_simple marshal_namespace marshal_recode

compile: compile.ml
	ocamlfind ocamlc -g -custom -o compile -package pxp,str -linkpkg compile.ml

marshal_simple: marshal_simple.ml
	ocamlfind ocamlc -g -custom -o marshal_simple -package pxp,str -linkpkg marshal_simple.ml

marshal_namespace: marshal_namespace.ml
	ocamlfind ocamlc -g -custom -o marshal_namespace -package pxp,str -linkpkg marshal_namespace.ml

marshal_recode: marshal_recode.ml
	ocamlfind ocamlc -g -custom -o marshal_recode -package pxp -linkpkg marshal_recode.ml


#----------------------------------------------------------------------
.PHONY: all
all:

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa sample sample.ml out1 out2
	rm -f *.d1 *.d2 *.d3 *.out *.out1 *.out2

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f compile marshal_simple marshal_namespace marshal_recode

