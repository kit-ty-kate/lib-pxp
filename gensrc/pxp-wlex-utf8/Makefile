TOP_DIR = ../..

include $(TOP_DIR)/Makefile.rules

ENC = utf8
INPUT = $(TOP_DIR)/src/pxp-lex
MLL = $(shell echo pxp_wlex_$(ENC)_*.mll)
CMO = $(MLL:.mll=.cmo)
CMX = $(MLL:.mll=.cmx)
OCAMLC_OPTIONS += -I $(TOP_DIR)/src/pxp-engine
OCAMLOPT_OPTIONS += $(OCAMLC_OPTIONS)
PACKAGES = netstring,wlexing

.PHONY: all opt generate clean CLEAN distclean install uninstall

all: pxp_wlex_$(ENC).cma pxp_wlex_link_$(ENC).cmo

opt: pxp_wlex_$(ENC).cmxa pxp_wlex_link_$(ENC).cmx

pxp_wlex_$(ENC).cma: $(CMO)
	$(OCAMLC) -o pxp_wlex_$(ENC).cma -a $(CMO)

pxp_wlex_$(ENC).cmxa: $(CMX)
	$(OCAMLOPT) -o pxp_wlex_$(ENC).cmxa -a $(CMX)

generate: gen_done

gen_done: $(INPUT)/*.def $(INPUT)/*.src
	$(LEXPP) -charclasses "$(INPUT)/char_classes_wlex.def" \
		-lexsrc "$(INPUT)/lex.src" \
		-linksrc "$(INPUT)/link_wlex.src" \
		-encoding "$(ENC)" \
		-outformat wlex \
		-outlexprefix "pxp_wlex_$(ENC)" \
		-outlinkprefix "pxp_wlex_link_$(ENC)"
	for mll in *.mll; do echo "Doing $$mll:"; $(WLEX) $$mll; done
	touch gen_done

clean:
	rm -f $(CLEAN_LIST) *.ml *.mll gen_done

CLEAN: clean

# distclean is not clean, because the distribution contains the .ml files
# and gen_done
distclean:
	rm -f $(CLEAN_LIST) META

install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a pxp_wlex_link_*.cmo pxp_wlex_link_*.cmx pxp_wlex_link_*.o META` && \
        $(OCAMLFIND) install pxp-wlex-$(ENC) $$files

uninstall:
	$(OCAMLFIND) remove pxp-wlex-$(ENC)
