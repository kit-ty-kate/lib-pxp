# ----------------------------------------------------------------------
# Configuration section
# ----------------------------------------------------------------------

# NOTE: The following settings work for _me_. It is normally not necessary
# to build the documentation in stable releases. However, you may try it...

# Location of the "readme" executable. It is contained in the example
# tree of the distribution.

#readme = ../examples/readme/readme
readme = readme

# Where to install DEV by "make DEV":

installdev = $$HOME/homepage/ocaml-programming.de/packages/documentation/pxp/index_dev.html

installrel = $$HOME/homepage/ocaml-programming.de/packages/documentation/pxp/index_rel.html

# ----------------------------------------------------------------------
# make all
# make DEV


.PHONY: all
all: README INSTALL ABOUT-FINDLIB SPEC EXTENSIONS

README: README.xml common.xml config.xml readme.dtd
	$(readme) -text README.xml >README

INSTALL: INSTALL.xml common.xml config.xml readme.dtd
	$(readme) -text INSTALL.xml >INSTALL

ABOUT-FINDLIB: ABOUT-FINDLIB.xml common.xml config.xml readme.dtd
	$(readme) -text ABOUT-FINDLIB.xml >ABOUT-FINDLIB

SPEC: SPEC.xml common.xml config.xml readme.dtd
	$(readme) -text SPEC.xml >SPEC

EXTENSIONS: EXTENSIONS.xml common.xml config.xml readme.dtd
	$(readme) -text EXTENSIONS.xml >EXTENSIONS

DEV: DEV.xml common.xml config.xml readme.dtd
	$(readme) -text DEV.xml >DEV
	$(readme) -html DEV.xml >$(installdev)

REL: RELEASE-NOTES.xml common.xml config.xml readme.dtd
	$(readme) -text RELEASE-NOTES.xml >RELEASE-NOTES
	$(readme) -html RELEASE-NOTES.xml >$(installrel)

config.xml:
	touch config.xml

common.xml:
	ln -s dist-common.xml common.xml 

readme.dtd:
	ln -s ../examples/readme/readme.dtd .

.PHONY: clean
clean:

.PHONY: CLEAN
CLEAN: clean
	$(MAKE) -C manual CLEAN

.PHONY: distclean
distclean: clean
	rm -f *~
	cp common.xml dist-common.xml
	$(MAKE) -C manual distclean
