.PHONY: all
all: README INSTALL ABOUT-FINDLIB

README: README.xml common.xml
	readme -text README.xml >README

INSTALL: INSTALL.xml common.xml
	readme -text INSTALL.xml >INSTALL

ABOUT-FINDLIB: ABOUT-FINDLIB.xml common.xml
	readme -text ABOUT-FINDLIB.xml >ABOUT-FINDLIB

.PHONY: clean
clean:

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C manual CLEAN

.PHONY: distclean
distclean: clean
	rm -f *~
	$(MAKE) -C manual distclean

.PHONY: symlinks
symlinks:
	ln -s ../examples/readme/readme.dtd .

