.PHONY: all
all: README INSTALL ABOUT-FINDLIB SPEC EXTENSIONS dev

README: README.xml common.xml config.xml
	readme -text README.xml >README

INSTALL: INSTALL.xml common.xml config.xml
	readme -text INSTALL.xml >INSTALL

ABOUT-FINDLIB: ABOUT-FINDLIB.xml common.xml config.xml
	readme -text ABOUT-FINDLIB.xml >ABOUT-FINDLIB

SPEC: SPEC.xml common.xml config.xml
	readme -text SPEC.xml >SPEC

EXTENSIONS: EXTENSIONS.xml common.xml config.xml
	readme -text EXTENSIONS.xml >EXTENSIONS

.PHONY: dev
dev: DEV.xml
	readme -html DEV.xml >$$HOME/homepage/ocaml-programming.de/packages/documentation/pxp/index_dev.html

config.xml:
	touch config.xml

common.xml:
	ln -s dist-common.xml common.xml 

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

