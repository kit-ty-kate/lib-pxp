# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

all: create_element modify strip

create_element: create_element.ml
	ocamlfind ocamlc -labels -g -custom -o create_element -package pxp -linkpkg create_element.ml

modify: modify.ml
	ocamlfind ocamlc -g -custom -o modify -package pxp -linkpkg modify.ml

strip: strip.ml
	ocamlfind ocamlc -g -custom -o strip -package pxp -linkpkg strip.ml

#----------------------------------------------------------------------
.PHONY: all
all:

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa out.xml

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f create_element modify strip
