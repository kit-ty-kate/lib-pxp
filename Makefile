# make all: compiles the configured packages with ocamlc
# make opt: compiles the configured packages with ocamlopt
# make install: installs the configured packages
# make clean: cleans everything up

NAME=pxp
TOP_DIR=.

include Makefile.rules

.PHONY: build
build: all
	if ocamlopt 2>/dev/null; then $(MAKE) opt; fi

.PHONY: all
all:
	$(MAKE) -C tools all
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg all || exit; done
	for pkg in $(GENPKGLIST); do $(MAKE) -C gensrc/$$pkg clean || exit; done
	for pkg in $(GENPKGLIST); do $(MAKE) -C gensrc/$$pkg generate || exit; done
	for pkg in $(GENPKGLIST); do $(MAKE) -C gensrc/$$pkg all || exit; done

.PHONY: opt
opt:
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg opt || exit; done
	for pkg in $(GENPKGLIST); do $(MAKE) -C gensrc/$$pkg opt || exit; done

# The following are for development:
.PHONY: lexers
lexers:
	for pkg in $(GENPKGLIST); do $(MAKE) -C gensrc/$$pkg generate || exit; done
	for pkg in $(GENPKGLIST); do $(MAKE) -C gensrc/$$pkg all || exit; done

.PHONY: lexers-again
lexers-again:
	for pkg in $(GENPKGLIST); do $(MAKE) -C gensrc/$$pkg clean || exit; done
	$(MAKE) lexers


# The following PHONY rule is important for Cygwin:
.PHONY: install
install:
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg install || exit; done
	for pkg in $(GENPKGLIST); do $(MAKE) -C gensrc/$$pkg install || exit; done

.PHONY: uninstall
uninstall:
	$(MAKE) -C src uninstall	
	for pkg in $(ALLGENPKGLIST); do $(OCAMLFIND) remove $$pkg; done

# On the toplevel, clean is CLEAN:

.PHONY: clean
clean:
	$(MAKE) -C tools CLEAN
	$(MAKE) -C src CLEAN
	for dir in gensrc/pxp-*; do $(MAKE) -C $$dir CLEAN || true; done
	$(MAKE) -C examples CLEAN
	$(MAKE) -C rtests CLEAN
	rm -f .testscript .testout

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean:
	rm -f *~ Makefile.conf
	$(MAKE) -C tools distclean
	$(MAKE) -C src distclean
	$(MAKE) -C examples distclean
	$(MAKE) -C rtests distclean
	test ! -f doc/Makefile || $(MAKE) -C doc distclean
	for dir in gensrc/pxp-*; do if [ -f $$dir/gen_dir ]; then rm -rf $$dir; else $(MAKE) -C $$dir distclean; fi; done

.PHONY: RELEASE
RELEASE:
	./configure -version >RELEASE

# for oasis
.PHONY: postconf
postconf:
	echo 'pkg_version="$(VERSION)"' >>setup.data
