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

OTHERSRC = open_pxp_lex_aux_utf8.src \
	   open_pxp_lex_misc_utf8.src \
	   pxp_lex_defs_utf8.def \
	   pxp_lex_aux_utf8.ml

FROM_GENERIC = $(LEXERSRC) pxp_lex_defs_generic.def

LEXERML = $(LEXERSRC:.src=_utf8.ml)

OBJ = pxp_lex_aux_utf8.cmo \
      $(LEXERSRC:.src=_utf8.cmo)

XOBJ = $(OBJ:.cmo=.cmx)

all:
	$(MAKE) symlinks
	$(MAKE) insertion_done
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code pxp_lex_utf8.cma

opt:
	$(MAKE) symlinks
	$(MAKE) insertion_done
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code pxp_lex_utf8.cmxa

symlinks:
	for x in $(FROM_GENERIC); do test -f $$x || ln -s $(GENERIC)/$$x .; done
	test -f pxp_lex_aux_utf8.ml || ln -s $(GENERIC)/pxp_lex_aux.ml pxp_lex_aux_utf8.ml

insertion_done: $(LEXERSRC) $(OTHERSRC)
	$(INSERT_VARIANT) -variant utf8 $(LEXERSRC)
	touch insertion_done

generate: $(LEXERML)

clean:
	rm -f $(CLEAN_LIST) $(LEXERML) *.mll
	rm -f $(FROM_GENERIC)
	rm -f pxp_lex_aux_utf8.ml
	rm -f insertion_done
	rm -f pxp_lex_defs_utf8.def
	rm -f META

CLEAN: clean

distclean: clean

depend: *.ml
	$(OCAMLDEP) *.ml >depend


pxp_lex_defs_utf8.def: pxp_lex_defs_generic.def pxp_lex_defs_drv_utf8.def
	$(UCS2_TO_UTF8) pxp_lex_defs_generic.def pxp_lex_defs_utf8.def
	cat pxp_lex_defs_drv_utf8.def >>pxp_lex_defs_utf8.def

install:
	files=`$(COLLECT_FILES) *.cmi *.cma *.cmxa *.a pxp_lex_link_utf8.cmo pxp_lex_link_utf8.cmx  pxp_lex_link_utf8.o META` && \
	$(OCAMLFIND) install pxp-lex-utf8 $$files

uninstall:
	$(OCAMLFIND) remove pxp-lex-utf8
