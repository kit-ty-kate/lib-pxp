xhtml2html: main.ml
	ocamlfind ocamlc -o xhtml2html -package pxp -linkpkg main.ml


.PHONY: clean
clean:
	rm -f *.cmi *.cmo

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~ xhtml2html

