library.cgi: library.ml
	ocamlfind ocamlc -o library.cgi library.ml \
		-package "pxp,pxp-pp,cgi" -syntax camlp4o,byte -linkpkg

clean:
	rm *.cmo *.cmi *.cgi
