TOP_DIR = ../..

include $(TOP_DIR)/Makefile.rules

.PHONY: all opt generate clean CLEAN distclean install uninstall

all:

opt:

generate:

clean:
	rm -f $(CLEAN_LIST)

CLEAN: clean

distclean: clean

install:
	$(OCAMLFIND) install pxp-wlex META

uninstall:
	$(OCAMLFIND) remove pxp-wlex
