# make readme:		make bytecode executable
# make readme.opt: 	make native executable
# make clean: 		remove intermediate files
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files
# make release: 	cleanup, create archive, tag CVS module 
#			(for developers)
#----------------------------------------------------------------------

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


