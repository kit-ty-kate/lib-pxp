.PHONY: all
all: README INSTALL ABOUT-FINDLIB SPEC PRERELEASE EXTENSIONS

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

PRERELEASE: PRERELEASE.xml common.xml config.xml
	readme -text PRERELEASE.xml >PRERELEASE

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

