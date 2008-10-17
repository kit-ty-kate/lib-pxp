all:
	$(MAKE) -C m2parsergen
#	$(MAKE) -C ucs2_to_utf8
	$(MAKE) -C lexpp
	$(MAKE) -C odoc

clean:

CLEAN: clean
	$(MAKE) -C m2parsergen CLEAN
#	$(MAKE) -C ucs2_to_utf8 CLEAN
	$(MAKE) -C lexpp CLEAN
	$(MAKE) -C odoc CLEAN

distclean:
	$(MAKE) -C m2parsergen distclean
#	$(MAKE) -C ucs2_to_utf8 distclean
	$(MAKE) -C lexpp distclean
	$(MAKE) -C odoc distclean
