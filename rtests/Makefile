-include ../Makefile.conf

.PHONY: all
all: toploops
	top=`dirname $$PWD` &&                     \
	OCAMLPATH="$$top/gensrc:$$top/src:$$OCAMLPATH" && \
	export OCAMLPATH &&                        \
	$(MAKE) -C reader &&                       \
	$(MAKE) -C write &&                        \
	$(MAKE) -C codewriter &&                   \
	$(MAKE) -C canonxml &&                     \
	$(MAKE) -C negative &&                     \
	$(MAKE) -C dynamic &&                      \
	$(MAKE) -C lex

.PHONY: toploops
toploops: exit.cmo
	if echo $(GENPKGLIST) | grep pxp-lex-utf8 >/dev/null 2>/dev/null; \
	  then $(MAKE) toploop.lex-utf8; fi
	if echo $(GENPKGLIST) | grep pxp-wlex-utf8 >/dev/null 2>/dev/null; \
	  then $(MAKE) toploop.wlex-utf8; fi
	if echo $(GENPKGLIST) | grep pxp-ulex-utf8 >/dev/null 2>/dev/null; \
	  then $(MAKE) toploop.ulex-utf8; fi

toploops-again:
	rm -f toploop.*
	$(MAKE) toploops

toploop.lex-utf8: 
	top=`dirname $$PWD` &&                                 \
	OCAMLPATH="$$top/gensrc:$$top/src:$$OCAMLPATH" &&      \
	export OCAMLPATH &&                                    \
	ocamlfind ocamlmktop -o toploop.lex-utf8 -linkpkg -package threads,pxp-engine,pxp-lex-iso88591,pxp-lex-utf8,findlib,str -thread

toploop.wlex-utf8: 
	top=`dirname $$PWD` &&                                 \
	OCAMLPATH="$$top/gensrc:$$top/src:$$OCAMLPATH" &&      \
	export OCAMLPATH &&                                    \
	ocamlfind ocamlmktop -o toploop.wlex-utf8 -linkpkg -package threads,pxp-engine,pxp-lex-iso88591,pxp-wlex-utf8,str -thread

toploop.ulex-utf8: 
	top=`dirname $$PWD` &&                                 \
	OCAMLPATH="$$top/gensrc:$$top/src:$$OCAMLPATH" &&      \
	export OCAMLPATH &&                                    \
	ocamlfind ocamlmktop -o toploop.ulex-utf8 -linkpkg -package threads,pxp-engine,pxp-lex-iso88591,pxp-ulex-utf8,str -thread

exit.cmo:
	ocamlc -c exit.ml

.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa toploop.*

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
