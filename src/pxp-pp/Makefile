TOP_DIR = ../..

include $(TOP_DIR)/Makefile.rules

PACKAGES = netstring,ulex,camlp4.quotations,camlp4.macro

OCAMLC_OPTIONS += -syntax camlp4o
OCAMLC_OPTIONS += $(CAMLP4_LOC)


all: pxp_pp.cma

opt:

pxp_pp.cma: pxp_pp.cmo
	$(OCAMLC) -a -o pxp_pp.cma pxp_pp.cmo

clean:
	rm -f $(CLEAN_LIST)

CLEAN: clean

distclean: clean
	rm -f META depend

install:
	$(OCAMLFIND) install pxp-pp pxp_pp.cma META

uninstall:
	$(OCAMLFIND) remove pxp-pp
