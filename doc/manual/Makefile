DOCBOOK_HTML = /usr/share/sgml/docbkdsl/html
DOCBOOK_PRINT = /usr/share/sgml/docbkdsl/print
SRC = $(PWD)/src

.PHONY: html ps

default: html ps

html: html/book1.htm html/pic/done

ps: ps/markup.ps ps/pic/done


src/readme.ent: ../../examples/readme/to_html.ml
	src/getcode.ml <../../examples/readme/to_html.ml >src/readme.ent

src/yacc.mli.ent: ../../pxp_yacc.mli
	src/getcode.ml <../../pxp_yacc.mli >src/yacc.mli.ent

src/dtd.mli.ent: ../../pxp_dtd.mli
	src/getcode.ml <../../pxp_dtd.mli >src/dtd.mli.ent

html/book1.htm: src/*.sgml src/readme.ent src/yacc.mli.ent src/dtd.mli.ent
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

ps/markup.ps: ps/markup.dvi
	cd ps; \
	dvips -f <markup.dvi >markup.ps

ps/pic/done: src/pic/*.fig
	mkdir -p ps/pic
	l=`cd src/pic; echo *.fig`; \
	for x in $$l; do fig2dev -L ps -m 0.8 src/pic/$$x ps/pic/`basename $$x .fig`.ps; done
	touch ps/pic/done

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
