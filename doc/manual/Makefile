DOCBOOK_HTML = /usr/share/sgml/docbkdsl/html
DOCBOOK_PRINT = /usr/share/sgml/docbkdsl/print
SRC = $(PWD)/src

.PHONY: html ps

default: html ps

html: html/book1.htm

ps: ps/markup.ps


src/readme.ent: ../../examples/readme/to_html.ml
	src/getcode.ml <../../examples/readme/to_html.ml >src/readme.ent

src/yacc.mli.ent: ../../markup_yacc_shadow.mli
	src/getcode.ml <../../markup_yacc_shadow.mli >src/yacc.mli.ent

src/dtd.mli.ent: ../../markup_dtd.mli
	src/getcode.ml <../../markup_dtd.mli >src/dtd.mli.ent

html/book1.htm: src/*.sgml src/readme.ent src/yacc.mli.ent src/dtd.mli.ent
	mkdir -p html
	cp src/markup.css html; \
	cd html; \
	rm -f *.htm*; \
	jade -t sgml -D$(DOCBOOK_HTML) -D$(SRC) markup.sgml; \
	true

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

ps/markup.dvi: ps/markup.tex
	cd ps; \
	jadetex markup.tex; \
	jadetex markup.tex; \
	jadetex markup.tex

ps/markup.ps: ps/markup.dvi
	cd ps; \
	dvips -f <markup.dvi >markup.ps

.SUFFIXES: .xml .sgml

.sgml.xml: 
	sx -xndata $< >$@; true



clean:
	rm -rf html man ps
	rm -f src/readme.ent

CLEAN: clean

distclean:
	rm -f src/*~
	rm -f *~
	rm -f ps/*.aux ps/*.dvi ps/*.log ps/*.tex
