# make all: compiles the configured packages with ocamlc
# make opt: compiles the configured packages with ocamlopt
# make install: installs the configured packages
# make clean: cleans everything up

# Inclusion of Makefile.conf may fail when cleaning up:
-include Makefile.conf

NAME=pxp
TOP_DIR=.

.PHONY: all
all:
	$(MAKE) -C tools all
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg all || exit; done

.PHONY: opt
opt:
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg opt || exit; done

# The following PHONY rule is important for Cygwin:
.PHONY: install
install:
	for pkg in $(PKGLIST); do $(MAKE) -C src/$$pkg install || exit; done

.PHONY: uninstall
uninstall:
	$(MAKE) -C src uninstall	

.PHONY: clean
clean:
	rm -f Makefile.conf
	$(MAKE) -C tools CLEAN
	$(MAKE) -C src CLEAN
	$(MAKE) -C examples CLEAN
	$(MAKE) -C rtests CLEAN

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean:
	rm -f Makefile.conf
	$(MAKE) -C tools distclean
	$(MAKE) -C src distclean
	$(MAKE) -C examples distclean
	$(MAKE) -C rtests distclean

RELEASE:
	./configure -version >RELEASE
	v=`cat RELEASE` && test "x$$v" = "$(VERSION)"

.PHONY: dist
dist: RELEASE
	cd ..; gtar czf $(NAME)-$(VERSION).tar.gz --exclude='*/CVS*' --exclude="*/*~" --exclude="*/Makefile.conf" --exclude="*/META" --exclude="*/depend" --exclude="*reptil*" --exclude="*/doc/common.xml" --exclude="*/doc/config.xml" --exclude="*/doc/readme.dtd" --exclude="*.fig.bak" --exclude="*/ps/pic*" --exclude="*/Mail*" $(NAME)/*

.PHONY: tag-release
tag-release: RELEASE
	@echo Checking whether everything is checked in...
	! cvs -n update 2>&1 | grep '^[MACPU] '
	r=`head -1 RELEASE | sed -e s/\\\./-/g`; cd ..; cvs tag -F $(NAME)-$$r pxp

.PHONY: release
release: distclean
	$(MAKE) tag-release
	$(MAKE) dist

