all:
	false

clean:

distclean:
	$(MAKE) -C pxp distclean
	$(MAKE) -C pxp-engine distclean
	$(MAKE) -C pxp-lex-iso88591 distclean
	$(MAKE) -C pxp-lex-utf8 distclean
	$(MAKE) -C pxp-wlex distclean

CLEAN:
	$(MAKE) -C pxp CLEAN
	$(MAKE) -C pxp-engine CLEAN
	$(MAKE) -C pxp-lex-iso88591 CLEAN
	$(MAKE) -C pxp-lex-utf8 CLEAN
	$(MAKE) -C pxp-wlex CLEAN

uninstall:
	$(MAKE) -C pxp uninstall
	$(MAKE) -C pxp-engine uninstall
	$(MAKE) -C pxp-lex-iso88591 uninstall
	$(MAKE) -C pxp-lex-utf8 uninstall
	$(MAKE) -C pxp-wlex uninstall
