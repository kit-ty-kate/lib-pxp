# make all: 		make bytecode executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)

#----------------------------------------------------------------------

SRC = ast.ml lexer.ml parser.ml generator.ml
OBJ = $(SRC:.ml=.cmo)

#----------------------------------------------------------------------


.PHONY: all
all: m2parsergen

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa lexer.ml parser.ml \
	      parser.mli

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~ depend depend.pkg m2parsergen a.out x.ml

#----------------------------------------------------------------------
# general rules:

OPTIONS   =
OCAMLC    = ocamlc -g $(OPTIONS) $(ROPTIONS)
OCAMLOPT  = ocamlopt -p $(OPTIONS) $(ROPTIONS)
OCAMLDEP  = ocamldep $(OPTIONS)
OCAMLFIND = ocamlfind

#----------------------------------------------------------------------

depend: $(SRC) 
	$(OCAMLDEP) $(SRC) >depend

m2parsergen: $(OBJ)
	$(OCAMLC) -o m2parsergen $(OBJ)

.SUFFIXES: .cmo .cmi .cmx .ml .mli .mll .mly

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc $<

include depend
