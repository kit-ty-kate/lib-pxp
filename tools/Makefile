TOP_DIR = ..
include $(TOP_DIR)/Makefile.rules

all:
	$(MAKE) -C src
	test -f m2parsergen$(EXEC_SUFFIX) || ln -s src/m2parsergen/m2parsergen$(EXEC_SUFFIX) .
	#test -f ucs2_to_utf8$(EXEC_SUFFIX) || ln -s src/ucs2_to_utf8/ucs2_to_utf8$(EXEC_SUFFIX) .
	test -f lexpp$(EXEC_SUFFIX) || ln -s src/lexpp/lexpp$(EXEC_SUFFIX)

clean:
	rm -f m2parsergen$(EXEC_SUFFIX) ucs2_to_utf8$(EXEC_SUFFIX)

CLEAN: clean
	$(MAKE) -C src CLEAN

distclean: clean
	$(MAKE) -C src distclean
