TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules

all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

generate: pxp_wlex.ml

pxp_wlex.ml: pxp_wlex.mll
	wlex pxp_wlex.mll

depend: *.ml
	ocamldep *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST) pxp_wlex.ml

CLEAN: clean

# pxp_wlex.ml is contained in the distribution such that users
# need not to patch ocaml sources to make wlex.

distclean:
	test -f pxp_wlex.ml
	rm -f $(CLEAN_LIST)
