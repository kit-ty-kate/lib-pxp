# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

all: compile marshal_simple marshal_namespace marshal_recode

compile: xmlcompile.ml
	ocamlfind ocamlc -g -a -o compile.cma -package pxp,str xmlcompile.ml
	../create-wrapper compile

marshal_simple: marshal_simple.ml
	ocamlfind ocamlc -g -a -o marshal_simple.cma -package pxp,str marshal_simple.ml
	../create-wrapper marshal_simple

marshal_namespace: marshal_namespace.ml
	ocamlfind ocamlc -g -a -o marshal_namespace.cma -package pxp,str marshal_namespace.ml
	../create-wrapper marshal_namespace

marshal_recode: marshal_recode.ml
	ocamlfind ocamlc -g -a -o marshal_recode.cma -package pxp marshal_recode.ml
	../create-wrapper marshal_recode


#----------------------------------------------------------------------
.PHONY: all
all:

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa sample sample.ml out1 out2
	rm -f *.d1 *.d2 *.d3 *.out *.out1 *.out2
	rm -f compile marshal_simple marshal_namespace marshal_recode

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~

