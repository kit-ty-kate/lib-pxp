# make xmlforms:	make bytecode executable
# make xmlforms.opt: 	make native executable
# make clean: 		remove intermediate files
# make CLEAN:           remove intermediate files (recursively)
# make distclean: 	remove any superflous files
# make release: 	cleanup, create archive, tag CVS module 
#			(for developers)
#----------------------------------------------------------------------

.PHONY: xmlforms
xmlforms:
	$(MAKE) -f Makefile.code xmlforms

.PHONY: xmlforms.opt
xmlforms.opt:
	$(MAKE) -f Makefile.code xmlforms.opt


.PHONY: clean
clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C styles CLEAN

.PHONY: distclean
distclean: clean
	rm -f *~ depend depend.pkg
	rm -f xmlforms xmlforms.opt
	$(MAKE) -C styles distclean


