# make readme:		make bytecode executable
# make readme.opt: 	make native executable
# make clean: 		remove intermediate files
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files
# make install
#----------------------------------------------------------------------

BIN = /usr/local/bin

.PHONY: readme
readme:
	$(MAKE) -f Makefile.code readme

.PHONY: readme.opt
readme.opt:
	$(MAKE) -f Makefile.code readme.opt


.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C styles CLEAN

.PHONY: distclean
distclean: clean
	rm -f *~ depend depend.pkg
	rm -f readme readme.opt
	$(MAKE) -C styles distclean

.PHONY: install
install:
	cp readme $(BIN)
