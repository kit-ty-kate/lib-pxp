# make all: compiles the configured packages with ocamlc
# make opt: compiles the configured packages with ocamlopt
# make install: installs the configured packages
# make clean: cleans everything up

# Inclusion of Makefile.conf may fail when cleaning up:

-include Makefile.conf

all:
	$(MAKE) -C tools all
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg all || exit; done

opt:
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg opt || exit; done

install:
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg install || exit; done

uninstall:
	$(MAKE) -C src uninstall	

clean:
	rm -f Makefile.conf
	$(MAKE) -C tools CLEAN
	$(MAKE) -C src CLEAN
	$(MAKE) -C examples CLEAN
	$(MAKE) -C rtests CLEAN

CLEAN: clean

distclean:
	$(MAKE) -C tools distclean
	$(MAKE) -C src distclean
	$(MAKE) -C examples distclean
	$(MAKE) -C rtests distclean
