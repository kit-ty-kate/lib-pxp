pull.cma: pull.ml
	ocamlfind ocamlc -a -o pull.cma -package "pxp,camlp4" \
		-syntax camlp4o \
		pull.ml

clean:
	rm -rf *.cmi *.cmo *.cma

CLEAN: clean

distclean: clean
