# make all: 		make bytecode executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)

#----------------------------------------------------------------------

TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules

SRC = ast.ml lexer.ml parser.ml generator.ml
OBJ = $(SRC:.ml=.cmo)

#----------------------------------------------------------------------

.PHONY: all
all: m2parsergen$(EXEC_SUFFIX)

.PHONY: clean
clean:
	rm -f $(CLEAN_LIST) lexer.ml parser.ml parser.mli
	rm -f depend m2parsergen$(EXEC_SUFFIX) a.out x.ml

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean

depend: $(SRC) 
	$(OCAMLDEP) $(SRC) >depend

m2parsergen$(EXEC_SUFFIX): $(OBJ)
	$(OCAMLC) -o m2parsergen$(EXEC_SUFFIX) $(OBJ)

# Include dependencies:

include depend
