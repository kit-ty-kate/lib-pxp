TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules

all: lexpp$(EXEC_SUFFIX)

DEPOBJS = lexpp_file.ml uni_lexer.ml uni_parser.ml uni_parser.mli \
	  uni_types.ml ucs2_to_utf8.ml main.ml

OBJS = uni_types.cmo uni_lexer.cmo uni_parser.cmo lexpp_file.cmo \
	mll_lexer.cmo \
       ucs2_to_utf8.cmo main.cmo

PACKAGES=netstring

uni_lexer.ml:
	$(OCAMLLEX) uni_lexer.mll

mll_lexer.ml:
	$(OCAMLLEX) mll_lexer.mll

uni_parser.ml:
	$(OCAMLYACC) uni_parser.mly

uni_parser.mli:
	$(OCAMLYACC) uni_parser.mly

depend: $(DEPOBJS)
	$(OCAMLDEP) $(DEPOBJS) > depend

lexpp$(EXEC_SUFFIX): $(OBJS)
	$(OCAMLC) -o lexpp$(EXEC_SUFFIX) -linkpkg $(OBJS)

lexpp.cma: $(OBJS)
	$(OCAMLC) -o lexpp.cma -a $(OBJS)

clean:
	rm -f $(CLEAN_LIST) uni_lexer.ml uni_parser.ml uni_parser.mli \
	mll_lexer.ml \
	lexpp$(EXEC_SUFFIX) depend

CLEAN: clean

distclean: clean

include depend
