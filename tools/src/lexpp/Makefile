TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules

all: lexpp$(EXEC_SUFFIX)

DEPOBJS = file.ml lexer.ml parser.ml parser.mli types.ml ucs2_to_utf8.ml \
	  main.ml

OBJS = types.cmo lexer.cmo parser.cmo file.cmo ucs2_to_utf8.cmo main.cmo

PACKAGES=netstring

lexer.ml:
	$(OCAMLLEX) lexer.mll

parser.ml:
	$(OCAMLYACC) parser.mly

parser.mli:
	$(OCAMLYACC) parser.mly

depend: $(DEPOBJS)
	$(OCAMLDEP) $(DEPOBJS) > depend
 
lexpp$(EXEC_SUFFIX): $(OBJS)
	$(OCAMLC) -o lexpp$(EXEC_SUFFIX) -linkpkg $(OBJS)

lexpp.cma: $(OBJS)
	$(OCAMLC) -o lexpp.cma -a $(OBJS)

clean:
	rm -f $(CLEAN_LIST) lexer.ml parser.ml parser.mli \
	lexpp$(EXEC_SUFFIX)

CLEAN: clean

distclean: clean

include depend
