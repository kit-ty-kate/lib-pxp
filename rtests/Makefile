.PHONY: all
all:
	$(MAKE) -C canonxml

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C canonxml clean
	$(MAKE) -C negative clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm dumpfiles
	$(MAKE) -C canonxml distclean
	$(MAKE) -C negative distclean

dumpfiles: dumpfiles.ml
	ocamlc -o dumpfiles dumpfiles.ml
