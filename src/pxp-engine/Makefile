TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules

all: 
	cp pxp_lib_ocamlc.ml pxp_lib.ml
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt: 
	cp pxp_lib_ocamlopt.ml pxp_lib.ml
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

generate: pxp_core_parser.ml pxp_lexing.ml

.PHONY: depend
depend: 
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST) pxp_core_parser.ml pxp_lib.ml pxp_lexing.ml

CLEAN: clean

distclean: clean
	rm -f META depend

install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a pxp_top.cmo META` && \
	$(OCAMLFIND) install pxp-engine $$files

uninstall:
	$(OCAMLFIND) remove pxp-engine

pxp_lexing.ml: pxp_lexing.mlp
	$(IFDEF) $(LEXBUF_307) pxp_lexing.mlp
