TOP_DIR = ../..
ENC = utf8
INPUT = $(TOP_DIR)/src/pxp-lex
ML = $(shell echo pxp_ulex_$(ENC)_*.ml)
CMO = $(ML:.ml=.cmo)
CMX = $(ML:.ml=.cmx)
OCAMLC_OPTIONS = -I $(TOP_DIR)/src/pxp-engine -syntax camlp4o
OCAMLOPT_OPTIONS = $(OCAMLC_OPTIONS) -syntax camlp4o
PACKAGES = netstring,ulex

include $(TOP_DIR)/Makefile.rules

.PHONY: all opt generate clean CLEAN distclean install uninstall

all: pxp_ulex_$(ENC).cma pxp_ulex_link_$(ENC).cmo

opt: pxp_ulex_$(ENC).cmxa pxp_ulex_link_$(ENC).cmx

pxp_ulex_$(ENC).cma: $(CMO)
	$(OCAMLC) -o pxp_ulex_$(ENC).cma -a $(CMO)

pxp_ulex_$(ENC).cmxa: $(CMX)
	$(OCAMLOPT) -o pxp_ulex_$(ENC).cmxa -a $(CMX)

generate: gen_done

gen_done: $(INPUT)/*.def $(INPUT)/*.src
	$(LEXPP) -charclasses "$(INPUT)/char_classes_generic.def" \
		-lexsrc "$(INPUT)/lex.src" \
		-linksrc "$(INPUT)/link_ulex.src" \
		-encoding "$(ENC)" \
	        -outformat ulex \
		-outlexprefix "pxp_ulex_$(ENC)" \
		-outlinkprefix "pxp_ulex_link_$(ENC)"
	touch gen_done

clean:
	rm -f $(CLEAN_LIST) *.ml *.mll gen_done

CLEAN: clean

# distclean is not clean, because the distribution contains the .ml files
# and gen_done
distclean:
	rm -f $(CLEAN_LIST) META

install:
	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a pxp_ulex_link_*.cmo pxp_ulex_link_*.cmx pxp_ulex_link_*.o META` && \
        $(OCAMLFIND) install pxp-ulex-$(ENC) $$files

uninstall:
	$(OCAMLFIND) remove pxp-ulex-$(ENC)
