# make validate:        make bytecode executable
# make validate.opt:    make native executable
# make clean: 		remove intermediate files (in this directory)
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files (recursively)
#----------------------------------------------------------------------

all: create_element modify strip symmetry

create_element: create_element.ml
	ocamlfind ocamlc -labels -g -a -o create_element.cma -package pxp create_element.ml
	../create-wrapper create_element

modify: modify.ml
	ocamlfind ocamlc -g -a -o modify.cma -package pxp modify.ml
	../create-wrapper modify

strip: strip.ml
	ocamlfind ocamlc -g -a -o strip.cma -package pxp strip.ml
	../create-wrapper strip

symmetry: symmetry.ml
	ocamlfind ocamlc -g -a -o symmetry.cma -package pxp symmetry.ml
	../create-wrapper symmetry

#----------------------------------------------------------------------
.PHONY: all
all:

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa out.xml
	rm -f create_element modify strip

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~
