.PHONY: all
all:

.PHONY: clean
clean:

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C xmlforms CLEAN
	$(MAKE) -C validate CLEAN
	$(MAKE) -C readme CLEAN

.PHONY: distclean
distclean: clean
	rm -f *~
	$(MAKE) -C xmlforms distclean
	$(MAKE) -C validate distclean
	$(MAKE) -C readme distclean
