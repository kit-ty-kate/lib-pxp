.PHONY: all
all:
	$(MAKE) -C recode
	$(MAKE) -C reader
	$(MAKE) -C write
	$(MAKE) -C codewriter
	$(MAKE) -C canonxml
	$(MAKE) -C negative

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C recode clean
	$(MAKE) -C reader clean
	$(MAKE) -C write clean
	$(MAKE) -C codewriter clean
	$(MAKE) -C canonxml clean
	$(MAKE) -C negative clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f dumpfiles
	$(MAKE) -C recode distclean
	$(MAKE) -C reader distclean
	$(MAKE) -C write distclean
	$(MAKE) -C codewriter distclean
	$(MAKE) -C canonxml distclean
	$(MAKE) -C negative distclean

dumpfiles: dumpfiles.ml
	ocamlc -o dumpfiles dumpfiles.ml
