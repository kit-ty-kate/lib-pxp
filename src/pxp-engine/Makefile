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

generate: pxp_yacc.ml

depend: 
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST) META

CLEAN: clean

distclean: clean

install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a pxp_top.cmo META` && \
	$(OCAMLFIND) install pxp-engine $$files

uninstall:
	$(OCAMLFIND) remove pxp-engine
