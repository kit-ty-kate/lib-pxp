# Note: The following Makefile works for _me_. The stable releases of
# PXP contain an already built manual, so you do not need this Makefile.

# The following software is required:
# - jade/sp
# - docbook 3.x (Davenport Group)
# - docbook stylesheets by Normal Walsh
#
# Furthermore, you need an ocaml toploop "ocamlfattop" that can be
# built by:
# ocamlfind ocamlmktop -o ocamlfattop -package str,unix,findlib -linkpkg

DOCBOOK_HTML = /usr/share/sgml/docbkdsl/html
DOCBOOK_PRINT = /usr/share/sgml/docbkdsl/print
SRC = $(PWD)/src

SOURCE = ../../src/pxp-engine/pxp_document.mli \
         ../../src/pxp-engine/pxp_types.mli
UPSOURCE = $(shell for x in $(SOURCE); do echo "../$$x"; done)

.PHONY: html ps

default: html ps

html: html/book1.htm html/pic/done

ps: ps/pxp.ps ps/pic/done


src/readme.ent: ../../examples/readme/to_html.ml
	src/getcode.ml <../../examples/readme/to_html.ml >src/readme.ent

src/yacc.mli.ent: ../../src/pxp-engine/pxp_yacc.mli
	src/getcode.ml <../../src/pxp-engine/pxp_yacc.mli >src/yacc.mli.ent

src/dtd.mli.ent: ../../src/pxp-engine/pxp_dtd.mli
	src/getcode.ml <../../src/pxp-engine/pxp_dtd.mli >src/dtd.mli.ent

src/extracted.ent: $(SOURCE)
	mkdir -p src/extracted
	rm -f src/extracted/*
	(cd src; ./extract.ml $(UPSOURCE))

html/book1.htm: src/*.sgml src/readme.ent src/yacc.mli.ent src/dtd.mli.ent src/extracted.ent
	mkdir -p html
	cp src/markup.css html; \
	cd html; \
	rm -f *.htm*; \
	jade -t sgml -D$(DOCBOOK_HTML) -D$(SRC) -ihtml markup.sgml; \
	true
	touch html/TIMESTAMP

html/pic/done: src/pic/*.fig
	mkdir -p html/pic
	l=`cd src/pic; echo *.fig`; \
	for x in $$l; do fig2dev -L gif src/pic/$$x html/pic/`basename $$x .fig`.gif; done
	touch html/pic/done

#man: src/findlib_reference.xml
#	mkdir -p man
#	cd man; \
#	rm -f *.[0-9]; \
#	db2man <../src/findlib_reference.xml

ps/markup.tex: src/*.sgml src/readme.ent src/yacc.mli.ent src/dtd.mli.ent
	mkdir -p ps
	cd ps; \
	jade -t tex -D$(DOCBOOK_PRINT) -D$(SRC) markup.sgml; \
	true

ps/markup.dvi: ps/markup.tex ps/pic/done
	cd ps; \
	jadetex markup.tex; \
	jadetex markup.tex; \
	jadetex markup.tex

ps/pxp.ps: ps/markup.dvi
	cd ps; \
	dvips -f <markup.dvi >pxp.ps

ps/pic/done: src/pic/*.fig
	mkdir -p ps/pic
	l=`cd src/pic; echo *.fig`; \
	for x in $$l; do fig2dev -L ps -m 0.8 src/pic/$$x ps/pic/`basename $$x .fig`.ps; done
	touch ps/pic/done

.SUFFIXES: .xml .sgml

.sgml.xml: 
	sx -xndata $< >$@; true

.PHONY: odoc
odoc: html/pic/done
	cd ../../src/pxp-engine && $(MAKE) doc
	rm -rf html/ref
	mkdir -p html/ref
	cp style.css html/ref
	ocamldoc -v -g ../../tools/src/odoc/chtml.cmo \
		-t "PXP Reference" \
		-I ../../src/pxp-engine \
		-load ../../src/pxp-engine/pxp_engine.dump \
		-d html/ref \
		-colorize-code \
		-css-style style.css -intro index.txt



clean:
	rm -rf html man ps
	rm -f src/readme.ent

CLEAN: clean

distclean:
	rm -f src/*~
	rm -f *~
	rm -f ps/*.aux ps/*.dvi ps/*.log ps/*.tex
