.PHONY: all
all: README INSTALL ABOUT-FINDLIB

README: README.xml common.xml config.xml
	readme -text README.xml >README

INSTALL: INSTALL.xml common.xml config.xml
	readme -text INSTALL.xml >INSTALL

ABOUT-FINDLIB: ABOUT-FINDLIB.xml common.xml config.xml
	readme -text ABOUT-FINDLIB.xml >ABOUT-FINDLIB

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

