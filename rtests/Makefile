.PHONY: all
all:
	top=`dirname $$PWD` &&                     \
	OCAMLPATH="$$top/gensrc:$$top/src" &&      \
	export OCAMLPATH &&                        \
	$(MAKE) -C reader &&                       \
	$(MAKE) -C write &&                        \
	$(MAKE) -C codewriter &&                   \
	$(MAKE) -C canonxml &&                     \
	$(MAKE) -C negative &&                     \
	$(MAKE) -C dynamic                         \
	$(MAKE) -C lex

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C reader clean
	$(MAKE) -C write clean
	$(MAKE) -C codewriter clean
	$(MAKE) -C canonxml clean
	$(MAKE) -C negative clean
	$(MAKE) -C dynamic clean
	$(MAKE) -C lex clean

.PHONY: distclean
distclean: clean
	rm -f *~
	rm -f dumpfiles
	$(MAKE) -C reader distclean
	$(MAKE) -C write distclean
	$(MAKE) -C codewriter distclean
	$(MAKE) -C canonxml distclean
	$(MAKE) -C negative distclean
	$(MAKE) -C dynamic distclean
	$(MAKE) -C lex distclean

dumpfiles: dumpfiles.ml
	ocamlc -o dumpfiles dumpfiles.ml
