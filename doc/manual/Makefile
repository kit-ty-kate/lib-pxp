DOCBOOK_HTML = /usr/share/sgml/docbkdsl/html
DOCBOOK_PRINT = /usr/share/sgml/docbkdsl/print
SRC = $(PWD)/src

.PHONY: html tex man

default: html man
# 'tex' still not working

html: html/book1.htm
tex: tex/markup.dvi


src/readme.ent: ../../examples/readme/to_html.ml
	src/getcode.ml <../../examples/readme/to_html.ml >src/readme.ent

html/book1.htm: src/*.sgml src/readme.ent
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

tex/markup.tex: src/*.sgml src/readme.ent
	mkdir -p tex
	cd tex; \
	jade -t tex -D$(DOCBOOK_PRINT) -D$(SRC) markup.sgml; \
	true

tex/markup.dvi: tex/markup.tex
	cd tex; \
	jadetex markup.tex; \
	jadetex markup.tex

.SUFFIXES: .xml .sgml

.sgml.xml: 
	sx -xndata $< >$@; true



clean:
	rm -rf html man tex
	rm -f src/*.xml

distclean:
	rm -f src/*.xml
	rm -f src/*~
	rm -f *~
