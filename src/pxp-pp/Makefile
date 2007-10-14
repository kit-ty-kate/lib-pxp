TOP_DIR = ../..

include $(TOP_DIR)/Makefile.rules

PACKAGES = netstring,ulex,camlp4.quotations,camlp4.macro

OCAMLC_OPTIONS += -syntax camlp4o
OCAMLC_OPTIONS += $(CAMLP4_LOC) -ppopt -loc -ppopt loc


all: pxp_pp.cma

opt:

pxp_pp.cma: pxp_pp.cmo
	$(OCAMLC) -a -o pxp_pp.cma pxp_pp.cmo

pxp_pp.ml: pxp_pp.ml.$(CAMLP4_STYLE)
	cp pxp_pp.ml.$(CAMLP4_STYLE) pxp_pp.ml

clean:
	rm -f $(CLEAN_LIST)

CLEAN: clean

distclean: clean
	rm -f META depend

install:
	$(OCAMLFIND) install pxp-pp pxp_pp.cma META

uninstall:
	$(OCAMLFIND) remove pxp-pp
