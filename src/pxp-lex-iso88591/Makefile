TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules

GENERIC = ../pxp-lex-generic

LEXERSRC = pxp_lex_misc.src \
           pxp_lex_document.src \
           pxp_lex_content.src \
           pxp_lex_within_tag.src \
           pxp_lex_document_type.src \
           pxp_lex_declaration.src \
           pxp_lex_dtd_string.src \
           pxp_lex_content_string.src \
           pxp_lex_name_string.src

OTHERSRC = open_pxp_lex_aux_iso88591.src \
	   open_pxp_lex_misc_iso88591.src \
	   pxp_lex_defs_iso88591.def \
	   pxp_lex_aux_iso88591.ml

FROM_GENERIC = $(LEXERSRC)

LEXERML = $(LEXERSRC:.src=_iso88591.ml)

OBJ = pxp_lex_aux_iso88591.cmo \
      $(LEXERSRC:.src=_iso88591.cmo)

XOBJ = $(OBJ:.cmo=.cmx)

all:
	$(MAKE) symlinks
	$(MAKE) insertion_done
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code pxp_lex_iso88591.cma

opt:
	$(MAKE) symlinks
	$(MAKE) insertion_done
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code pxp_lex_iso88591.cmxa

symlinks:
	for x in $(FROM_GENERIC); do test -f $$x || ln -s $(GENERIC)/$$x .; done
	test -f pxp_lex_aux_iso88591.ml || ln -s $(GENERIC)/pxp_lex_aux.ml pxp_lex_aux_iso88591.ml

insertion_done: $(LEXERSRC) $(OTHERSRC)
	$(INSERT_VARIANT) -variant iso88591 $(LEXERSRC)
	touch insertion_done

generate: $(LEXERML)

clean:
	rm -f $(CLEAN_LIST) $(LEXERML) *.mll
	rm -f $(FROM_GENERIC)
	rm -f pxp_lex_aux_iso88591.ml
	rm -f insertion_done
	rm -f META

CLEAN: clean

distclean: clean

depend: *.ml
	$(OCAMLDEP) *.ml >depend

install:
	files=`$(COLLECT_FILES) *.cmi *.cma *.cmxa *.a pxp_lex_link_iso88591.cmo pxp_lex_link_iso88591.cmx  pxp_lex_link_iso88591.o META` && \
	$(OCAMLFIND) install pxp-lex-iso88591 $$files

uninstall:
	$(OCAMLFIND) remove pxp-lex-iso88591
