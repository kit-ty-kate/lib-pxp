.PHONY: all
all:
	$(MAKE) -C recode
	$(MAKE) -C reader
	$(MAKE) -C canonxml
	$(MAKE) -C negative

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C recode clean
	$(MAKE) -C reader clean
	$(MAKE) -C canonxml clean
	$(MAKE) -C negative clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f dumpfiles
	$(MAKE) -C recode distclean
	$(MAKE) -C reader distclean
	$(MAKE) -C canonxml distclean
	$(MAKE) -C negative distclean

dumpfiles: dumpfiles.ml
	ocamlc -o dumpfiles dumpfiles.ml
